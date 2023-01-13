{-# LANGUAGE Safe #-}

{-|

Module      : Player
Description : (Untrusted) Players.

This untrusted module implements the server side of a player, which
communicates with its client side via TCP. When the server prompts the
client for input, it uses \"resign\" as the quit message (see
"Command").

-}

module Player
       (PlacingHandle,
        ShootingHandle,
        placing,
        getComplete,
        ShotResult(..),
        shoot,
        opponentShot,
        won,
        lost)
       where

import qualified Data.Char as Char
import qualified Data.Maybe as Maybe

import LIO
import LIO.Concurrent
import LIO.DCLabel

import Sys
import qualified Aux
import qualified Command
import qualified Ship
import qualified Board
import qualified BoardUntrusted
import qualified ShotBoard

-- fatal error message on behalf of player

fatl :: String -> DC a

fatl = Sys.fatal "player"

coorReg :: String

-- regular expression describing a legal board coordinate

coorReg = ['[', 'a', '-', Char.chr(Char.ord 'a' + Board.size - 1), ']']

getShotPos :: Handle -> DC(Maybe Board.Pos)

-- getShotPos clnt returns a DC action that, when run, gets a shot position
-- (or resignation) from the user (via clnt); Nothing means resignation;
-- otherwise, Just of the position is returned

getShotPos clnt = do
  psRes <-
    Command.promptServer
    clnt
    ("take your next shot: (" ++ coorReg ++ coorReg ++ ")")
    "resign"
  case psRes of
    Nothing       -> return Nothing
    Just "resign" -> return Nothing
    Just x        ->
      case Board.strToPos x of
        Nothing -> do
          Command.infoServer clnt ["syntax error"]
          getShotPos clnt
        res     -> return res

parsePosOrient :: String -> Maybe(Board.Pos, Board.Orient)

-- parse a ship position plus orientation; yields Nothing when
-- parsing fails

parsePosOrient x =
      case Aux.tokens (\ c -> c == ':') x of
        [y, z] ->
          (case Board.strToPos y of
              Nothing  -> Nothing
              Just pos ->
                case Board.strToOrient z of
                  Nothing     -> Nothing
                  Just orient -> Just(pos, orient))
        _      -> Nothing

getShipPosOrient :: Handle -> Ship.Ship -> DC(Maybe(Board.Pos, Board.Orient))

-- getShipPosOrient clnt returns a DC action that, when run, gets a
-- ship position plus orientation -- or resignation -- from the user
-- (via clnt); Nothing means resignation; otherwise, Just of the result
-- is returned

getShipPosOrient clnt ship = do
  psRes <-
    Command.promptServer
    clnt
    ("place your " ++ Ship.shipToStr ship ++
     " (" ++ "size " ++ show(Ship.size ship) ++
     ", " ++ coorReg ++ coorReg ++ ":[h+v])")
    "resign"
  case psRes of
    Nothing       -> return Nothing
    Just "resign" -> return Nothing
    Just x        ->
      case parsePosOrient x of
        Nothing -> do
          Command.infoServer clnt ["syntax error"]
          getShipPosOrient clnt ship
        res     -> return res

-- | Shot results communicated to shooting player.

data ShotResult =
       Repeat          -- ^ An attempt to shoot a position already shot.
     | Miss            -- ^ Miss -- no ship hit.
     | Hit             -- ^ Hit an unspecified ship.
     | Sank Ship.Ship  -- ^ Sank a specified ship -- last of its cells hit.

-- shooting phase commands

data Cmd =
       Shoot (DCMVar(Maybe(Board.Pos, ShotResult -> DC())))
     | OpponentShot Board.Pos
     | Won
     | Lost

-- | /Placing handle/ for player -- handle for player during the
-- placing phase of game.

data PlacingHandle =
  PlacingHandle (DCMVar(Maybe(Board.Complete, ShootingHandle)))

-- | /Shooting handle/ for player -- handle for player during the shooting
-- phase of game.

data ShootingHandle = ShootingHandle (DCMVar Cmd)

shootingLoop ::
  Handle -> DCMVar Cmd -> BoardUntrusted.Board -> ShotBoard.Board -> DC()

-- shooting phase loop

shootingLoop clnt shootingMVar board shotBoard = do
  Command.infoServer clnt ["[Your Board]"]
  Command.infoServer clnt $ BoardUntrusted.boardToStrs board
  Command.infoServer clnt ["[Opponent's Board]"]
  Command.infoServer clnt $ ShotBoard.boardToStrs shotBoard
  cmd <- takeLMVar shootingMVar
  case cmd of
    Shoot replyMVar  -> do
      sp <- getShotPos clnt
      case sp of
        Nothing  -> do
          putLMVar replyMVar Nothing
          shootingLoop clnt shootingMVar board shotBoard
        Just pos -> do
          shotMVar <- newEmptyLMVar dcPublic
          putLMVar replyMVar $ Just(pos, putLMVar shotMVar)
          shotRes <- takeLMVar shotMVar
          case shotRes of
            Repeat    -> do
              Command.infoServer clnt ["shot repetition"]
              shootingLoop clnt shootingMVar board shotBoard
            Miss      -> do
              Command.infoServer clnt ["you missed a ship"]
              let shotBoard' = ShotBoard.miss shotBoard pos
              shootingLoop clnt shootingMVar board shotBoard'
            Hit       -> do
              Command.infoServer clnt ["you hit a ship"]
              let shotBoard' = ShotBoard.hit shotBoard pos
              shootingLoop clnt shootingMVar board shotBoard'
            Sank ship -> do
              Command.infoServer clnt ["you sank the " ++ Ship.shipToStr ship]
              let shotBoard' = ShotBoard.sank shotBoard pos ship
              shootingLoop clnt shootingMVar board shotBoard'
    OpponentShot pos -> do
      let (board', sh) = BoardUntrusted.shoot board pos
      case sh of
        BoardUntrusted.Repeat    -> fatl "player interface error"
        BoardUntrusted.Miss      ->
          Command.infoServer clnt
            ["opponent shot cell " ++ Board.posToStr pos ++ " but missed"]
        BoardUntrusted.Hit ship  ->
          Command.infoServer clnt
            ["opponent shot cell " ++ Board.posToStr pos ++
             " and hit your " ++ Ship.shipToStr ship]
        BoardUntrusted.Sank ship ->
          Command.infoServer clnt
            ["opponent shot cell " ++ Board.posToStr pos ++
             " and sank your " ++ Ship.shipToStr ship]
      shootingLoop clnt shootingMVar board' shotBoard
    Won              -> do
      Command.infoServer clnt ["you won!"]
      Command.doneServer clnt
    Lost             -> do
      Command.infoServer clnt ["you lost!"]
      Command.doneServer clnt

placingLoop ::
  Handle -> DCMVar(Maybe(Board.Complete, ShootingHandle)) ->
  Board.Placing -> [Ship.Ship] -> DC()

-- placing phase loop; ship list is ships left to place

placingLoop clnt placingMVar placing []             = do
  Command.infoServer clnt $
    BoardUntrusted.placingToStrs placing ++
    ["", "SHOOTING PHASE", ""]
  shootingMVar <- newEmptyLMVar dcPublic
  let complete = Maybe.fromJust $ Board.placingToComplete placing
  putLMVar placingMVar $ Just(complete, ShootingHandle shootingMVar)
  shootingLoop clnt shootingMVar (BoardUntrusted.completeToBoard complete)
    ShotBoard.empty
placingLoop clnt placingMVar placing (ship : ships) = do
  Command.infoServer clnt $ BoardUntrusted.placingToStrs placing
  spo <- getShipPosOrient clnt ship
  case spo of
    Nothing           -> do
      Command.doneServer clnt
      putLMVar placingMVar Nothing
    Just(pos, orient) ->
      case Board.place placing ship pos orient of
        Board.PlacingSuccess placing ->
          placingLoop clnt placingMVar placing ships
        Board.DuplicateShip          -> error "cannot happen"
        Board.OffBoard               -> do
          Command.infoServer clnt ["ship (partially) off board"]
          placingLoop clnt placingMVar placing (ship : ships)
        Board.OverlapsShip           -> do
          Command.infoServer clnt ["ship overlaps other ship"]
          placingLoop clnt placingMVar placing (ship : ships)

shipsInfo :: [String]

-- lines describing ships

shipsInfo =
  map
  (\ ship ->
    [Ship.shipToLetter ship] ++ " = " ++ Ship.shipToStr ship ++  " (size " ++
    show(Ship.size ship) ++ ")")
  Ship.ships

placing :: Handle -> Bool -> DC PlacingHandle

-- ^ @'placing' clnt goFirst@ returns a 'DC' action that, when run
-- with current label 'dcPublic', starts up a new player, returning
-- its placing handle, and leaving the current label and clearance
-- unchanged.  The player will use @clnt@ (which must have label
-- 'dcPublic') to communicate with its client side, using the protocol
-- of "Command", and will go (shoot) first iff @goFirst@.
--
-- /Note/: all 'DC' actions /derived/ from the placing handle via the
-- following functions must be run with current label 'dcPublic'. They
-- all leave the current label and clearance unchanged.  In
-- particular, this applies to the 'DC' actions returned by the reply
-- functions produced by the 'DC' actions returned by the @shoot@
-- function -- see below.

placing clnt goFirst = do
  placingMVar <- newEmptyLMVar dcPublic
  Command.infoServer clnt $
    ["Welcome to Battleship!",
     "",
     "At any prompt, you may type \"resign\" or signal end-of-file " ++
     "to resign.",
     "Ships:"] ++
     map (\ s -> "  " ++ s ++ ",") (Aux.allButLast shipsInfo) ++
     ["  " ++ last shipsInfo ++ "."] ++
     ["Cell positions consist of row then column; e.g., df is row d, " ++
      "column f.",
      "Ship placements consist of row/column of leftmost/topmost cell,",
      "  followed by h for horizontal or v for vertical.",
      "  E.g., df:h means horizontal placement with leftmost cell at df.",
      "  E.g., df:v means vertical placement with topmost cell at df.",
      "Shot cells:",
      "  * = miss,",
      "  uppercase/+ = hit.",
      "",
      "You are Player " ++ (if goFirst then "1" else "2"),
      "",
      "PLACING PHASE",
      ""]
  forkLIO $ placingLoop clnt placingMVar Board.empty Ship.ships
  return $ PlacingHandle placingMVar

getComplete :: PlacingHandle -> DC(Maybe(Board.Complete, ShootingHandle))

-- ^ @'getComplete' ph@ returns a 'DC' action that, when run, waits
-- for the player corresponding to the placing handle @ph@ to finish
-- placing its ships.  If the player resigns or terminates before
-- fully placing its ships, then the 'DC' action returns 'Nothing';
-- otherwise, the 'DC' action returns @'Just'(compl, sh)@, where
-- @compl@ is the complete placing chosen by the player, and @sh@
-- is the shooting handle for the player.  The 'DC' action returned by
-- calling 'getComplete' on a given placing handle may only be run
-- once.

getComplete(PlacingHandle mvar) = takeLMVar mvar

shoot :: ShootingHandle -> DC(Maybe(Board.Pos, ShotResult -> DC()))

-- ^ @'shoot' sh@ returns a 'DC' action that, when run, waits for
-- the player with shooting handle @sh@ to supply a value of type
-- @'Maybe'('Board.Pos', 'ShotResult' -> 'DC'())@, and then returns
-- this value.
--
-- * If 'Nothing' is supplied, this means the player is resigning.
--
-- * If @'Just'(pos, shotFun)@ is returned, this means the player has
-- chosen to shoot cell @pos@ of the opponent's board. The caller of
-- 'shoot' is then responsible for calling @shotFun@ with the value of
-- type 'ShotResult' describing the outcome of this shot, and for then
-- running the 'DC' action returned by @shotFun@, thus communicating this
-- outcome back to the player.  (This must be done before any other
-- 'DC' actions associated with @sh@ are run.)

shoot(ShootingHandle shootingMVar) = do
  replyMVar <- newEmptyLMVar dcPublic
  putLMVar shootingMVar (Shoot replyMVar)
  takeLMVar replyMVar
      
opponentShot :: ShootingHandle -> Board.Pos -> DC()

-- ^ @'opponentShot' sh pos@ returns a 'DC' action that, when
-- run, tells the player with shooting handle @sh@ that its
-- opponent has shot cell @pos@ of the player's board.

opponentShot (ShootingHandle shootingMVar) pos =
  putLMVar shootingMVar $ OpponentShot pos

won :: ShootingHandle -> DC()

-- ^ @'won' sh@ returns a 'DC' action that, when run, tells the
-- player with shooting handle @sh@ that it has won the game.  Once this
-- 'DC' action has been run, no 'DC' actions derived from shooting handle
-- @sh@ may be made.

won(ShootingHandle shootingMVar) = putLMVar shootingMVar Won

lost :: ShootingHandle -> DC()

-- ^ @'lost' sh@ returns a 'DC' action that, when run, tells the
-- player with shooting handle @sh@ that it has lost the game.  Once
-- this 'DC' action has been run, no 'DC' actions derived from shooting
-- handle @sh@ may be made.

lost(ShootingHandle shootingMVar) = putLMVar shootingMVar Lost
