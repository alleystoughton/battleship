{-# LANGUAGE Safe #-}

{-|

Module      : PlayerInterface
Description : (Trusted) Player Interfaces, Secure Against Each Other.

This trusted module implements player interfaces, which are secure
against each other.  Each player interface creates its own player (see
"Player").  Player interfaces communicate using the mechanism of
"PlayerInterfaceMsg".

-}

module PlayerInterface(playerInterface) where

import Data.List
import qualified Data.Maybe as Maybe
import qualified Control.Monad as Monad

import LIO
import LIO.Concurrent
import LIO.DCLabel

import Sys
import qualified Aux
import qualified Command
import qualified Ship
import qualified Board
import qualified ShotBoard
import qualified PlayerInterfaceMsg as PIM
import qualified Player

-- fatal error message on behalf of player interface

fatl :: String -> DC a

fatl = Sys.fatal "player interface"

catchFatl :: DC a -> DC a

catchFatl =
  Sys.catchFatal
  "player interface"
  "player raised exception"
  "player modified current label or clearance"

-- functions for manipulating labeled cells

-- used by originating PI to shoot a labeled cell (sent to it by the
-- receiving PI)

shootLC :: Board.LC -> DCPriv -> DC(Maybe(Board.Pos, Board.LSR, Board.LC))

shootLC (Board.LC lc) origPriv =
  let lab   = labelOf lc
      sec   = dcSecrecy lab
      integ = dcIntegrity lab
  in if sec == toCNF origPriv && integ `speaksFor` (toCNF origPriv)
     then do lc' <- relabelLabeledP origPriv (True %% integ) lc
             (origPrin, _, pos, shoot) <- unlabel lc'
             if toCNF origPrin == toCNF origPriv
               then shoot >>= \ lsr -> return $ Just(pos, lsr, Board.LC lc')
               else return Nothing
     else return Nothing

alreadyShotLC :: Board.LC -> Bool

-- used by receiving PI to test whether a labeled cell has already
-- been shot (is declassified)

alreadyShotLC(Board.LC lc) = dcSecrecy(labelOf lc) == cTrue

resultOfLC :: Board.LC -> Board.Pos -> Principal -> DC(Maybe Board.LSR)

-- used by receiving PI to harvest the result of a labeled cell
-- previously shot by the originating PI
--
-- if the originating PI has followed the protocol, the DC action of
-- the cell will already have been run. but whether or not this has
-- occurred will make no difference

resultOfLC (Board.LC lc) pos recvPrin =
  let lab   = labelOf lc
      sec   = dcSecrecy lab
      integ = dcIntegrity lab
  in if sec == cTrue && integ `speaksFor` (toCNF recvPrin)
     then do (_, recvPrin', pos', shoot) <- unlabel lc
             if recvPrin' == recvPrin && pos' == pos
               then shoot >>= return . Just
               else return Nothing
     else return Nothing

-- ownTurn and oppTurn are mutually tail recursive

ownTurn ::
  Player.ShootingHandle -> (PIM.PIMsg -> DC()) -> DC PIM.PIMsg ->
  Principal -> DCPriv -> [Ship.Ship] -> [Board.Pos] -> Board.LB ->
  [Ship.Ship] -> DC()

-- ownRem   = my unsunk ships
-- oppShots = shots made by opponent
-- oppRem   = opponent's unsunk ships

ownTurn sh send recv prin priv ownRem oppShots oppLB oppRem =
  if null ownRem -- last shot (if any) was by opponent
  then catchFatl $ Player.lost sh
  else do sr <- catchFatl $ Player.shoot sh
          case sr of
            Nothing          -> do
              send PIM.Resign
              catchFatl $ Player.lost sh
            Just(pos, reply) -> do
              let lc = Board.sub oppLB pos
              if alreadyShotLC lc
                then do catchFatl $ reply Player.Repeat
                        ownTurn sh send recv prin priv ownRem
                          oppShots oppLB oppRem
                else do send $ PIM.LC lc
                        msg <- recv
                        case msg of
                          PIM.LBC _          -> fatl "protocol error"
                          PIM.Resign         -> fatl "protocol error"
                          PIM.LC lc -> do
                            res <- resultOfLC lc pos prin
                            case res of
                              Nothing  -> fatl "protocol error"
                              Just lsr -> do
                                let oppLB' = Board.update oppLB pos lc
                                case lsr of
                                  Board.MissLSR      -> do
                                    catchFatl $ reply Player.Miss
                                    oppTurn sh send recv prin priv ownRem
                                      oppShots oppLB' oppRem
                                  Board.HitLSR       -> do
                                    catchFatl $ reply Player.Hit
                                    oppTurn sh send recv prin priv ownRem
                                      oppShots oppLB' oppRem
                                  Board.SankLSR ship -> do
                                    catchFatl $ reply $ Player.Sank ship
                                    oppTurn sh send recv prin priv ownRem
                                      oppShots oppLB' (delete ship oppRem)
                       
oppTurn ::
  Player.ShootingHandle -> (PIM.PIMsg -> DC()) -> DC PIM.PIMsg ->
  Principal -> DCPriv -> [Ship.Ship] -> [Board.Pos] -> Board.LB ->
  [Ship.Ship] -> DC()
      
-- ownRem   = my unsunk ships
-- oppShots = shots made by opponent
-- oppRem   = opponent's unsunk ships

oppTurn sh send recv prin priv ownRem oppShots oppLB oppRem =
  if null oppRem  -- last shot (if any) was by me
  then catchFatl $ Player.won sh
  else do msg <- recv
          case msg of
            PIM.LBC _          -> fatl "protocol error"
            PIM.Resign         -> catchFatl $ Player.won sh
            PIM.LC lc -> do
              shootRes <- shootLC lc priv
              case shootRes of
                Nothing            -> fatl "protocol error"
                Just(pos, lsr, lc) ->
                  if elem pos oppShots
                  then fatl "protocol error"
                  else do send $ PIM.LC lc
                          catchFatl $ Player.opponentShot sh pos
                          case lsr of
                            Board.SankLSR ship -> do
                              ownTurn sh send recv prin priv
                                (delete ship ownRem) (pos : oppShots)
                                oppLB oppRem
                            _                  -> do
                              ownTurn sh send recv prin priv ownRem
                                (pos : oppShots) oppLB oppRem

start ::
 Player.PlacingHandle -> (PIM.PIMsg -> DC()) -> DC PIM.PIMsg ->
 Bool -> DCPriv -> DC()

start ph send recv goFirst priv = do
  let prin = if goFirst then principal "player1" else principal "player2"
  gcRes <- catchFatl $ Player.getComplete ph
  case gcRes of
    Nothing         ->
      if goFirst
      then send PIM.Resign
      else do msg <- recv
              case msg of
                PIM.Resign -> return()
                _          -> send PIM.Resign
    Just(compl, sh) -> do
      lbcMay <- Board.completeToLBC compl prin priv
      case lbcMay of
        Nothing  -> fatl "cannot happen"
        Just lbc ->
          if goFirst
          then do send $ PIM.LBC lbc
                  msg <- recv
                  case msg of
                    PIM.Resign     -> catchFatl $ Player.won sh
                    PIM.LBC oppLBC -> do
                      resMay <- Board.lbcToLB oppLBC prin priv
                      case resMay of
                        Nothing    -> fatl "protocol error"
                        Just oppLB ->
                          ownTurn sh send recv prin priv Ship.ships []
                          oppLB Ship.ships
                    _              -> fatl "protocol error"
          else do msg <- recv
                  case msg of
                    PIM.Resign     -> catchFatl $ Player.won sh
                    PIM.LBC oppLBC -> do
                      resMay <- Board.lbcToLB oppLBC prin priv
                      case resMay of
                        Nothing    -> fatl "protocol error"
                        Just oppLB -> do
                          send $ PIM.LBC lbc
                          oppTurn sh send recv prin priv Ship.ships []
                            oppLB Ship.ships
                    _              -> fatl "protocol error"

playerInterface ::
  Handle -> (PIM.PIMsg -> DC()) -> DC PIM.PIMsg -> Bool -> DCPriv -> DC()

-- ^ @'playerInterface' clnt send recv goFirst priv@
-- returns a 'DC' action that, when run, starts a player interface.
--
-- * The current state must have label 'dcPublic' and clearance
-- @'cFalse' %% 'cTrue'@.
--
-- * @clnt@ is the handle used for communicating with the player's client
-- side; its label must be 'dcPublic'.
--
-- * @send@ and @recv@ are used for communicating with the opponent
-- player interface; they must work when the current label and
-- clearance are 'dcPublic' and clearance @'cFalse' %% 'cTrue'@, and
-- must not affect the current label or clearance.
--
-- * @goFirst@ is 'True' iff the player interface is supposed to take
-- the first (3rd, 5th, etc.) turn.
--
-- * @'toCNF' priv@ is @'toCNF' $ 'principal' \"player1\"@, if @goFirst@,
-- and is @'toCNF' $ 'principal' \"player2\"@, otherwise.
--
-- As the 'DC' action is run, the current label and clearance will be
-- maintained at 'dcPublic' and @'cFalse' %% 'cTrue'@, respectively.

playerInterface clnt send recv goFirst priv = do
  ph <- catchFatl $ Player.placing clnt goFirst
  start ph send recv goFirst priv
