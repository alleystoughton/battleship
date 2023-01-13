{-# LANGUAGE Safe #-}

{-|

Module      : ShotBoard
Description : (Untrusted) Tracking shots on Opponent's Board.

This untrusted module implements shot boards, which record what has
been learned about an opponent's board though shooting.

-}

module ShotBoard
       (Board,
        empty,
        miss,
        hit,
        sank,
        boardToStrs)
       where

import qualified Data.Char as Char
import Data.Maybe
import Data.List
import Data.Ord

import qualified Aux
import qualified Ship
import qualified Matrix
import qualified MatrixUntrusted
import qualified Board

-- a cell of a shot board

data Cell =
    UnShot                 -- not shot
  | Miss                   -- shot, but not part of ship
  | Hit (Maybe Ship.Ship)  -- Hit Nothing means shot cell of unspecified
                           -- ship; Hit(Just ship) means shot cell of ship

-- Cell is an instance of the typeclass MatrixUntrusted.Cell; c.f., boardToStrs

instance MatrixUntrusted.Cell Cell where

  toChar UnShot           = ' '
  toChar Miss             = '*'
  toChar (Hit Nothing)    = '+'
  toChar (Hit(Just ship)) = Char.toUpper(Ship.shipToLetter ship)

-- a placing consists of a ship, the position of its leftmost/topmost
-- cell, and its orientation

data Placing = Placing (Ship.Ship, Board.Pos, Board.Orient) deriving (Eq)

instance Ord Placing where

  compare (Placing(ship, pos, orient)) (Placing(ship', pos', orient')) =
    case compare ship ship' of
      LT -> LT
      EQ ->
        case compare orient orient' of
          LT -> LT
          EQ -> compare pos pos'
          GT -> GT
      GT -> GT

-- | A shot board records what has been learned about an opponent's
-- board though shooting.
--
-- If one starts with the empty shot board, and then uses 'miss',
-- 'hit' and 'sank' to record what is learned through shooting at a
-- sequence of distinct positions of the opponent's board, then the
-- shot board will remain consistent with the opponent's board.
--
-- No attempt is made to detect when a sequence of calls of 'miss',
-- 'hit' and 'sank' doesn't correspond to an actual shooting history
-- -- but the resulting shot board will be meaningless in such a case.
--
-- A cell of a shot board will either be:
--
--   * unshot;
--
--   * a miss -- shot, but not part of a ship;
--
--   * an unspecified hit -- shot, and part of an unknown (possibily shot)
--   ship;
--
--   * a specified hit -- shot, and part of a specified, sunk ship.
--
-- In addition to the matrix of cells, the shot board records the
-- possible locations of sunk ships: for each sunk ship, it is
-- guaranteed that one of the recorded possibilities is correct.

data Board = Board (Matrix.Matrix Cell, [Placing])

empty :: Board

-- ^A shot board in which all cells are unshot.

empty = Board(Matrix.const Board.size Board.size UnShot, [])

sub :: Matrix.Matrix Cell -> (Int, Int) -> Cell

sub rows (i, j) = Matrix.sub Board.size Board.size rows i j

update :: Matrix.Matrix Cell -> (Int, Int) -> Cell -> Matrix.Matrix Cell

update rows (i, j) cell = Matrix.update Board.size Board.size rows i j cell

placingToStr :: Placing -> String

placingToStr(Placing(ship, pos, orient)) =
  Ship.shipToStr ship ++ " could be at " ++
  Board.posToStr pos ++ ":" ++ Board.orientToStr orient

placingsToStrs :: [Placing] -> [String]

placingsToStrs placngs = map placingToStr (sort placngs)

placings :: Ship.Ship -> Board.Pos -> [Placing]

-- placings ship pos are the placings of ship, both horizontal and
-- vertical, that include position pos

placings ship pos =
      let siz = Ship.size ship

          shipBegins k =
            let xs = [k - siz + 1 .. k]
            in filter
               (\ n -> n >= 0 && n + siz - 1 < Board.size)
               xs

          (i, j) = Board.posToPair pos
          horizs =
            map
            (\ k -> Placing(ship, fromJust(Board.pairToPos(i, k)), Board.Horiz))
            (shipBegins j)
          verts  =
            map
            (\ k -> Placing(ship, fromJust(Board.pairToPos(k, j)), Board.Vert))
            (shipBegins i)
      in horizs ++ verts

consistent :: Matrix.Matrix Cell -> Placing -> Bool

consistent rows (Placing(ship, pos, Board.Horiz)) =
  let (i, j) = Board.posToPair pos
      stop   = j + Ship.size ship

      con k =
        k == stop ||
        (case sub rows (i, k) of
            Hit Nothing     -> True
            Hit(Just ship') -> ship' == ship
            _               -> False) &&
        con(k + 1)
  in con j
consistent rows (Placing(ship, pos, Board.Vert)) =
  let (i, j) = Board.posToPair pos
      stop   = i + Ship.size ship

      con k =
        k == stop ||
        (case sub rows (k, j) of
            Hit Nothing     -> True
            Hit(Just ship') -> ship' == ship
            _               -> False) &&
        con(k + 1)
      in con i

applyPlacing :: Matrix.Matrix Cell -> Placing -> Matrix.Matrix Cell

applyPlacing rows (Placing(ship, pos, Board.Horiz)) =
  let (i, j) = Board.posToPair pos
      stop   = j + Ship.size ship

      apply rows k =
        if k == stop
        then rows
        else apply (update rows (i, k) (Hit $ Just ship)) (k + 1)
      in apply rows j
applyPlacing rows (Placing(ship, pos, Board.Vert))  =
  let (i, j) = Board.posToPair pos
      stop   = i + Ship.size ship

      apply rows k =
        if k == stop
        then rows
        else apply (update rows (k, j) (Hit $ Just ship)) (k + 1)
      in apply rows i

splitPlacingsByShipUniqueness :: [Placing] -> ([Placing], [Placing])

splitPlacingsByShipUniqueness placngs =
  let split uniqs nonUniqs []                                           =
        (uniqs, nonUniqs)
      split uniqs nonUniqs ((placng @ (Placing(ship, _, _))) : placngs) =
        if any
           (\ (Placing(ship', _, _)) -> ship' == ship)
           placngs ||
           any
           (\ (Placing(ship', _, _)) -> ship' == ship)
           nonUniqs
        then split uniqs (placng : nonUniqs) placngs
        else split (placng : uniqs) nonUniqs placngs
  in split [] [] placngs

iterate :: Board -> Board

iterate(Board(rows, placngs)) =
      let placngs'          = filter (consistent rows) placngs
          (uniqs, nonUniqs) = splitPlacingsByShipUniqueness placngs'
      in if null uniqs
         then Board(rows, nonUniqs)
         else let apply []             rows = rows
                  apply (uniq : uniqs) rows =
                    apply uniqs (applyPlacing rows uniq)
              in ShotBoard.iterate(Board(apply uniqs rows, nonUniqs))

miss :: Board -> Board.Pos -> Board

-- ^ Mark a given cell as a miss; this doesn't change the record of
-- possible locations of sunk ships.

miss (Board(rows, placngs)) pos =
  Board(update rows (Board.posToPair pos) Miss, placngs)

hit :: Board -> Board.Pos -> Board

-- ^ Mark a given cell as an unspecified hit that didn't sink a ship;
-- this doesn't change the record of possible locations of sunk ships.

hit (Board(rows, placngs)) pos =
  Board(update rows (Board.posToPair pos) (Hit Nothing), placngs)

sank :: Board -> Board.Pos -> Ship.Ship -> Board

-- ^ Indicate that shooting at the specified position sank the
-- specified ship.  The position of the cell matrix is recorded as a
-- hit of the ship, and the possible locations of the ship are added
-- to the record of possible sunk ship locations.
--
-- Then a loop is run, in which:
--
--   * possible sunk ship locations that are inconsistent with the cell
--   matrix are discarded;
--
--   * when only one possible location for a sunk ship remains, the matrix
--   is updated so that the ship's cells are all specified hits for that
--   ship.

sank (Board(rows, placngs)) pos ship =
  ShotBoard.iterate $ Board $
  (update rows (Board.posToPair pos) (Hit $ Just ship),
   placngs ++ placings ship pos)

boardToStrs :: Board -> [String]

-- ^ Convert a shot board to a list of strings describing it.
-- See "MatrixUntrusted" for the general format of the cell matrix. Cells
-- are presented as follows:
--
--   * an unshot cell is presented as a space;
--
--   * a miss is presented as @\"*\"@;
--
--   * an unspecified hit is presented as @\"+\"@;
--
--   * a specified hit is presented as the uppercase version of the
--   first letter of the ship's name.
--
-- The possible locations of sunk ships are described using the
-- @[a-z][a-z]:[h+v]@ notation.

boardToStrs(Board(rows, placngs)) =
  MatrixUntrusted.matrixToStrs Board.size Board.size rows ++
  placingsToStrs placngs
