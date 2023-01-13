{-# LANGUAGE Safe #-}

{-|

Module      : Board
Description : (Trusted) Placing-phase and Labeled Boards.

This trusted module implements placing-phase and labeled boards.
There are two types of placing-phase boards: placing boards, on which
some ships have been properly placed, and complete placing boards, on
which all ships have been properly placed.

A labeled cell of a player's labeled board is classified with the
player's principal, endorsed by both the player and opponent, and
includes the player's and opponent's principals and the cell's
position in its contents.  Thus it is safe to give the opponent
possession of the labeled board, assuring that the player can't change
its board during the shooting phase.  To avoid confusion, we refer to
the player as the *originating* player, and to the player's opponent
as the *receiving* player.  When the receiving player wants to shoot a
cell of the originating player's board, it sends the corresponding
labeled cell to the originating player, which declassifies it.  The
receiving player can then check that the declassified labeled cell
still has its endorsement, that the receiving principal of the
labeled cell is its own principal, and that its position is correct.

Because labeled cells contain the originating and receiving
principals, it is possible to tell a labeled cell originating from one
player from one originating from the other player.  Also included in a
labeled cell is a 'DC' action for shooting the labeled cell, returning
that result of the shot.  When a labeled cell is part of a ship, its
'DC' action uses a hidden 'LMVar' -- shared with the other cells of
the same ship -- to keep track of which of the ship's cells have been
hit, so far. Running a cell's 'DC' action more than once is guaranteed
to produce the same shooting result as the one obtained the first time
it was run.

-}

module Board
       (-- * Positions and Orientations
        size,
        Pos,
        posToPair,
        pairToPos,
        posToStr,
        strToPos,
        Orient(..),
        orientToStr,
        strToOrient,
        -- * Placing-phase Boards
        Placing,
        empty,
        isComplete,
        PlaceResult(..),
        place,
        Complete,
        placingToComplete,
        Membership(..), -- note out of order from where declared
        placingToMatrix,
        completeToMatrix,
        -- * Labeled Boards
        LSR(..),
        LC(..),
        LB,
        sub,
        update,
        LBC,
        completeToLBC,
        lbcToLB)
       where

import qualified Data.Char as Char
import Control.Monad
import Data.Monoid

import LIO
import LIO.DCLabel
import LIO.Concurrent

import Sys
import qualified Aux
import qualified Once
import qualified Ship
import qualified Matrix

size :: Int

-- ^ The board size is 10.  (This size must be between 1 and 26,
-- inclusive.  For the game to be playable, though, the size must be
-- big enough to accomodate all the ships of the "Ship" module.)

size = 10

-- | Abstract type of board positions, representing pairs @(i, j)@,
-- where @i@, @j@ are in the range @0@ @..@ @'size' - 1@.  @i@ indexes
-- a row, and @j@ indexes a column.

data Pos = Pos Int Int deriving (Eq, Ord)

swap :: Pos -> Pos

-- ^ Swap the coordinates of a position.

swap(Pos i j) = Pos j i

posToPair :: Pos -> (Int, Int)

-- ^ Converts a position to the pair it represents.

posToPair(Pos i j) = (i, j)

pairToPos :: (Int, Int) -> Maybe Pos

-- ^ Converts a pair to 'Just' of the position that represents it,
-- returning 'Nothing' when the pair isn't represented by a position.

pairToPos(i, j) =
      if i >= 0 && i < size &&
         j >= 0 && j < size
      then Just(Pos i j)
      else Nothing

letterToCoor :: Char -> Int

letterToCoor c = Char.ord c - Char.ord 'a'

coorToLetter :: Int -> Char

coorToLetter n = Char.chr(Char.ord 'a' + n)

posToStr :: Pos -> String

-- ^ Converts a position representing @(i, j)@ into the string @RC@,
-- where @R@ is the @i@th lowercase letter (counting from @0@), and
-- @C@ is the @j@th lowercase letter.

posToStr(Pos i j) = [coorToLetter i, coorToLetter j]

strToPos :: String -> Maybe Pos

-- ^ Converts a string to 'Just' of the position it describes, returning
-- 'Nothing' if it doesn't describe a position.

strToPos [x, y] =
  if Char.isLower x && Char.isLower y
  then let i = letterToCoor x
           j = letterToCoor y
       in if i < size && j < size
          then Just(Pos i j)
          else Nothing
  else Nothing
strToPos _      = Nothing

-- | A ship's orientation.

data Orient =
    Horiz -- ^ Horizontal.
  | Vert  -- ^ Vertical.
  deriving (Eq, Ord)

orientToStr :: Orient -> String

-- ^ Converts an orientation to a string: 'Horiz' goes to \"h\"
-- and 'Vert' goes to \"v\".

orientToStr Horiz = "h"
orientToStr Vert  = "v"

strToOrient :: String -> Maybe Orient

-- ^ Converts a string to 'Just' of the orientation it describes,
-- returning 'Nothing' when the string doesn't describe an
-- orientation.

strToOrient "h" = Just Horiz
strToOrient "v" = Just Vert
strToOrient _   = Nothing

-- | The membership of a board cell.

data Membership =
    Part Ship.Ship -- ^ Part of specified ship.
  | NotPart        -- ^ Not part of any ship.

notPart :: Membership -> Bool

-- is a cell not part of any ship?

notPart NotPart = True
notPart _       = False

-- | Abstract type consisting of a game board on which each ship has
-- been placed at most once, but where there is no information about
-- shooting.

data Placing = Placing (Matrix.Matrix Membership)

empty :: Placing

-- ^ A placing with no ships.

empty = Placing(Matrix.const size size NotPart)

hasShip :: Placing -> Ship.Ship -> Bool

-- test whether a placing contains a ship

hasShip (Placing rows) ship =
  any
  (\ row ->
    any
    (\ memb ->
      case memb of
        Part ship' -> ship' == ship
        _          -> False)
    row)
  rows

isComplete :: Placing -> Bool

-- ^ Tests whether a placing is /complete/, i.e., whether all ships have
-- been placed on the placing.

isComplete rows = all (hasShip rows) [minBound .. maxBound]

-- | Result of placing a ship.

data PlaceResult a =
       PlacingSuccess a  -- ^ Successful placing.
     | DuplicateShip     -- ^ Ship was already placed.
     | OffBoard          -- ^ Ship (partially) off the board.
     | OverlapsShip      -- ^ Ship overlaps another ship.

placeRow :: [Membership] -> Ship.Ship -> Int -> PlaceResult[Membership]

placeRow us ship j =
  let len = Ship.size ship
  in if j + len > size
     then OffBoard
     else let xs = take j us
              ys = drop j us
              zs = take len ys
              ws = drop len ys
          in if all notPart zs
             then PlacingSuccess $
                  xs ++
                  replicate len (Part ship) ++
                  ws
             else OverlapsShip

placeHoriz :: Placing -> Ship.Ship -> Pos -> PlaceResult Placing

placeHoriz (Placing rows) ship (Pos i j) =
  if hasShip (Placing rows) ship
  then DuplicateShip
  else let row = rows !! i
       in case placeRow row ship j of
            PlacingSuccess row -> PlacingSuccess(Placing(Aux.update rows i row))
            DuplicateShip      -> error "cannot happen"
            OffBoard           -> OffBoard
            OverlapsShip       -> OverlapsShip

place :: Placing -> Ship.Ship -> Pos -> Orient -> PlaceResult Placing

-- ^ Attempt to place a ship on a placing at a given position and with
-- a given orientation. When successful, returns 'PlacingSuccess' of the
-- resulting placing. Otherwise, the constructor indicates why the
-- placing failed.

place placing ship pos Horiz        = placeHoriz placing ship pos
place (Placing rows) ship pos Vert  =
  case placeHoriz (Placing(Matrix.transpose size size rows)) ship (swap pos) of
    PlacingSuccess(Placing rows) ->
      PlacingSuccess(Placing(Matrix.transpose size size rows))
    DuplicateShip                -> DuplicateShip
    OffBoard                     -> OffBoard
    OverlapsShip                 -> OverlapsShip

-- | Abstract type consisting of a complete placing.

data Complete = Complete (Matrix.Matrix Membership)

placingToComplete :: Placing -> Maybe Complete

-- ^ Returns 'Nothing' if the argument placing board isn't complete; otherwise
-- returns 'Just' of the placing board.

placingToComplete (Placing brd) =
  if isComplete(Placing brd) then Just(Complete brd) else Nothing

-- this is where Membership will be exported

placingToMatrix :: Placing -> Matrix.Matrix Membership

-- ^ Export a placing board to its membership matrix

placingToMatrix (Placing brd) = brd

completeToMatrix :: Complete -> Matrix.Matrix Membership

-- ^ Export a complete placing board to its membership matrix

completeToMatrix (Complete brd) = brd

-- labeled boards

-- | Result of shooting at a labeled cell -- a labeled shot result.

data LSR =
       MissLSR            -- ^ A miss.
     | HitLSR             -- ^ Hit an unspecified ship.
     | SankLSR Ship.Ship  -- ^ Sank a specified ship.

-- | Datatype of labeled cells.  A labeled cell has the form
-- @'LC' lc@,
-- where @lc@ is a 'DCLabel'ed value consisting of a 4-tuple
-- @(origPrin, recvPrin, pos, shoot)@:
--
-- * @origPrin :: 'Principal'@ is the originating player's principal;
--
-- * @recvPrin :: 'Principal'@ is the receiving player's principal;
--
-- * @pos :: 'Pos'@ is the cell's position; and
--
-- * @shoot :: 'DC' 'LSR'@ is a 'DC' action for shooting the cell and
-- reporting on the result (a value of type 'LSR').
--
-- Labeled cells are endorsed by both the originating and receiving
-- players; initially they are classified by the originating player.
--
-- When a cell is not part of a ship, its 'DC' action returns 'MissLSR'.
--
-- When a cell is part of a ship, its 'DC' action uses a hidden
-- 'LMVar', shared with the other cells of the same ship, to keep
-- track of which of the ship's cells have been hit, so far.  When
-- such a 'DC' action is run, it returns 'HitLSR' unless the cell
-- is/was the last of the ship's cells to be shot.  Running a cell's
-- 'DC' action more than once produces the same result as was produced
-- the first time it was run.

data LC =
  LC
  (DCLabeled
   (Principal,  -- originating principal
    Principal,  -- receiving principal
    Pos,        -- position of cell
    DC LSR      -- DC action for shooting cell
   ))

-- | Abstract type of labeled boards, consisting of labeled cells. They
-- have the same dimension as ordinary game boards.

data LB = LB (Matrix.Matrix LC)

sub :: LB -> Pos -> LC

-- ^ Selects a cell of a labeled board.

sub (LB rows) (Pos i j) = Matrix.sub size size rows i j

update :: LB -> Pos -> LC -> LB

-- ^ @'update' lb pos lc@ returns a labeled board that's identical
-- to @lb@ except that the labeled cell at position @pos@ is @lc@.

update (LB rows) (Pos i j) lc =
  LB $ Matrix.update size size rows i j lc

createShipMVars :: DC[DCMVar[Pos]]

-- createShipMVars is a DC action that returns a list of new LMVars
-- with label dcPublic and initial value [] -- one for each ship. An
-- exception will be raised if the current label isn't less than or
-- equal to dcPublic, or if dcPublic isn't less than or equal to the
-- current clearance.

createShipMVars =
  foldM
  (\ lMVars _ -> do
      lMVar <- newLMVar dcPublic []
      return $ lMVar : lMVars)
  []
  Ship.ships

-- | Abstract type of labeled board closures. See 'completeToLBC' and
-- 'lbcToLB'.

data LBC = LBC (Once.Once (Principal, DCPriv) (Maybe LB))

completeToLBC :: Complete -> Principal -> DCPriv -> DC(Maybe LBC)

-- ^ Turn a complete placing board into a labeled board closure, using
-- the originating player's principal and privilege.
--
-- @'completeToLBC' compl origPrin origPriv@ returns a 'DC' action
-- that behaves as follows. First, it checks that @'toCNF' origPrin ==
-- 'toCNF' origPriv@; if this isn't true, it returns 'Nothing'.  Next,
-- it checks whether the current label can flow to 'dcPublic', and
-- 'dcPublic' can flow to the current clearance.  If this isn't the
-- case, it returns 'Nothing'. Otherwise, it returns 'Just' of a
-- labeled board closure @lbc@.
--
-- @'lbcToLB' lbc recvPrin recvPriv@ returns a 'DC' action that
-- behaves as follows. First it checks that running @'guardWrite'
-- 'dcPublic'@ will succeed.  If this isn't the case, it returns
-- 'Nothing'. Otherwise, it runs @'guardWrite' 'dcPublic'@, which may
-- raise the current label.
--
-- If running a 'DC' action of the form @'lbcToLB' lbc prin priv@
-- (for some principal prin and privilege priv) has /previously/
-- gotten past this running of 'guardWrite', our 'DC' action
-- returns 'Nothing'.  Otherwise, it continues, as follows.
-- (This check is performed atomically -- guaranteeing that
-- only one execution of a 'DC' action of the form 
-- @'lbcToLB' lbc prin priv@ gets to this point.)
--
-- Next, the 'DC' action checks that @origPrin /= recvPrin@ and
-- @'toCNF' recvPrin == 'toCNF' recvPriv@. If this is not true, the
-- 'DC' action returns 'Nothing'.  Next, the 'DC' action checks that
-- combination of @origPriv@ and @recvPriv@ is strong enough, given
-- the current label and clearance, to create labeled values with
-- label @origPriv '%%' origPriv '/\' recvPriv@.  If this is not true,
-- the 'DC' action returns 'Nothing'.
--
-- Next, the 'DC' action creates a list -- one for each ship -- of
-- new, private 'LMVar's of type @['Board.Pos']@ with label 'dcPublic'
-- and initial value @[]@. A ship's 'LMVar' records the positions of
-- the cells of the ship that have been hit -- listed in reverse order
-- of shooting, with no duplicates.
--
-- Next, it converts the complete placing @compl@ into a labeled
-- board @lb@, transforming each cell of @compl@ into a 4-tuple as follows:
--
-- * A cell of ship @ship@ at position @(i, j)@ is turned into
-- @(origPrin, recvPrin, (i, j), shoot)@, where @shoot@ is a 'DC'
-- action that knows about the private 'LMVar' for @ship@. When
-- @shoot@ is run (which means the cell is being shot at), the
-- contents of the 'LMVar' is updated, if necessary, to include
-- position @(i, j)@. If the resulting number of positions in the
-- 'LMVar' is equal to the number of cells of @ship@, /and/ if @(i,
-- j)@ was the /last/ position to be added to the list of shot
-- positions, then @shoot@ returns @'SankLSR' ship@; otherwise, it
-- returns 'HitLSR'.  (When @shoot@ is run, the value of the 'LMVar'
-- is taken, using 'takeLMVar', and then put with a possibly updated
-- list of shot positions, using 'putLMVar'.  These operations will
-- result in @'guardWrite' 'dcPublic'@ being run. Depending upon the
-- current label and clearance, this may raise the current label or
-- even raise an exception.  Because of how the 'LMVar' is taken and
-- then put, running @shoot@ is thread safe.)
--
-- * A vacant cell at position @(i, j)@ is turned into @(origPrin,
-- recvPrin, (i, j), 'return' 'MissLSR')@.
--
-- Each 4-tuple @t@ is then turned into a labeled cell
-- @'LC' lc@, where the labeled value @lc@ consists of value @t@ and
-- label @origPriv '%%' origPriv '/\' recvPriv@.
--
-- Finally, @'Just' lb@ is returned.

completeToLBC (Complete rows) origPrin origPriv = do
  cur <- getLabel
  clr <- getClearance
  chk <- Sys.guardAlloc_Check dcPublic
  if toCNF origPrin == toCNF origPriv && chk
    then do once <-
              Once.makeOnce
              (\ (recvPrin, recvPriv) -> do
                  -- because of how invoked (see lbcToLB), the
                  -- current label is now equal to dcPublic
                  let lab      = origPriv %% origPriv /\ recvPriv
                      combPriv = origPriv `mappend` recvPriv
                  chk <- Sys.guardAllocP_Check combPriv lab
                  if origPrin /= recvPrin &&
                     toCNF recvPrin == toCNF recvPriv && chk
                    then do shipLMVars <- createShipMVars
                            lb <-
                              complToLB origPrin recvPrin combPriv lab
                              shipLMVars
                            return $ Just lb
                    else return Nothing)
            return $ Just $ LBC once
    else return Nothing
                  
  where complToLB origPrin recvPrin combPriv lab shipLMVars = do
          let rows' =
                Matrix.mapPos
                (\ i j memb ->
                  case memb of
                    Part ship ->
                      (origPrin, recvPrin, Pos i j,
                       let shipLMVar = shipLMVars !! Ship.ord ship
                       in do poss <- takeLMVar shipLMVar
                             let poss' =
                                   if elem (Pos i j) poss
                                   then poss
                                   else Pos i j : poss
                             putLMVar shipLMVar poss'
                             if length poss' == Ship.size ship &&
                                head poss' == Pos i j
                               then return $ SankLSR ship
                               else return HitLSR)
                    NotPart   -> (origPrin, recvPrin, Pos i j, return MissLSR))
                rows
          rows'' <-
            Matrix.mapM
            (\ cell -> labelP combPriv lab cell >>= return . LC)
            rows'
          return $ LB rows''
  
lbcToLB :: LBC -> Principal -> DCPriv -> DC(Maybe LB)

-- ^ Turn a labeled board closure into a labeled board, using the
-- receiving player's principal and privilege.  See 'completeToLBC'.

lbcToLB (LBC once) recvPrin recvPriv = do
  cur <- getLabel
  clr <- getClearance
  chk <- Sys.guardWrite_Check dcPublic
  if chk
    then do res <- Once.runOnce once (recvPrin, recvPriv)
            case res of
              Nothing       -> return Nothing
              Just Nothing  -> return Nothing
              Just(Just lb) -> return $ Just lb
    else return Nothing
