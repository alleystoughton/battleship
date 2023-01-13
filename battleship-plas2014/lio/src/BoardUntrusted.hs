{-# LANGUAGE Safe #-}

{-|

Module      : BoardUntrusted
Description : (Untrusted) Supplement to Board module.

This untrusted module implements the abstract type of shooting-phase
boards.  It also defines functions for describing boards as
strings and lists of strings.

-}

module BoardUntrusted
       (-- * Shooting-phase Boards
        Board,
        completeToBoard,
        allSunk,
        ShotResult(..),
        shoot,
        -- * String Descriptions of Boards
        placingToStrs,
        completeToStrs,
        boardToStrs)
       where

import qualified Data.Char as Char

import Sys
import qualified Aux
import qualified Ship
import qualified Matrix
import qualified MatrixUntrusted
import Board

-- boards of shooting phase

-- cells of shooting phase boards, consisting of membership plus
-- boolean saying whether shot or not (True = shot, False = not shot)

data Cell = Cell Membership Bool

-- | Abstract type of shooting phase boards.

data Board = Board (Matrix.Matrix Cell)

completeToBoard :: Complete -> Board

-- ^ Turns a complete placing into a board on which no shooting has
-- occurred.

completeToBoard compl =
  Board(Matrix.map (\ memb -> Cell memb False) (Board.completeToMatrix compl))

sunk :: Board -> Ship.Ship -> Bool

-- is a ship sunk on a board (all of its cells hit)?

sunk (Board rows) ship =
  foldl
  (\ n row ->
    foldl
    (\ m (Cell memb shot) ->
      case (memb, shot) of
        (Part ship', True) ->
          if ship' == ship then m + 1 else m
        _                  -> m)
    n
    row)
  0
  rows ==
  Ship.size ship

allSunk :: Board -> Bool

-- ^ Tests whether all ships of a board are sunk.

allSunk brd = all (sunk brd) Ship.ships

-- | Result of shooting at a board.

data ShotResult =
       Repeat          -- ^ An attempt to shoot a position already shot.
     | Miss            -- ^ Miss -- didn't hit ship.
     | Hit Ship.Ship   -- ^ Hit a specified ship.
     | Sank Ship.Ship  -- ^ Sank a specified ship -- last of its cells hit.

shootRow :: [Cell] -> Int -> ([Cell], ShotResult)

shootRow cells j =
  case cells !! j of
    Cell _           True  -> (cells, Repeat)
    Cell NotPart     False ->
      (Aux.update cells j (Cell NotPart True), Miss)
    Cell (Part ship) False ->
      (Aux.update cells j (Cell (Part ship) True), Hit ship)

shoot :: Board -> Pos -> (Board, ShotResult)

-- ^ Shoot a given position of a board, yielding a new board and a
-- shot result.  The shot result indicates the result of the shooting.

shoot (Board rows) pos =
  let (i, j)     = posToPair pos
      row        = rows !! i
      (row', sh) = shootRow row j
      rows'      = Aux.update rows i row'
      sh'        =
        case sh of
          Repeat   -> Repeat
          Miss     -> Miss
          Hit ship ->
            if sunk (Board rows') ship
            then Sank ship
            else Hit ship
          Sank _   -> error "cannot happen"
  in (Board rows', sh')

-- Membership is an instance of the typeclass MatrixUntrusted.Cell:

instance MatrixUntrusted.Cell Membership where

  toChar (Part ship) = Ship.shipToLetter ship
  toChar NotPart     = ' '

placingToStrs :: Placing -> [String]

-- ^ Convert a placing to a list of strings describing it.
-- See "MatrixUntrusted" for the general format. Each cell of a ship is
-- presented as the ship's first letter, in lowercase; vacant cells
-- are presented as @\" \"@.

placingToStrs plac =
  MatrixUntrusted.matrixToStrs size size (Board.placingToMatrix plac)

completeToStrs :: Complete -> [String]

-- ^ Convert a complete placing to a list of strings describing it --
-- works identically to 'placingToStrs'.

completeToStrs plac =
  MatrixUntrusted.matrixToStrs size size (Board.completeToMatrix plac)

-- Cell is an instance of the typeclass MatrixUntrusted.Cell:

instance MatrixUntrusted.Cell Cell where

  toChar(Cell (Part ship) False) = Ship.shipToLetter ship
  toChar(Cell (Part ship) True)  = Char.toUpper(Ship.shipToLetter ship)
  toChar(Cell NotPart     False) = ' ' 
  toChar(Cell NotPart     True)  = '*'

boardToStrs :: Board -> [String]

-- ^ Convert a board to a list of strings describing it.
-- See "MatrixUntrusted" for the general format.
--
-- * Each vacant cell (not part of a ship) that's not been shot at is
-- presented as @\" \"@.
--
-- * Each vacant cell that's been shot at is labeled @\"*\"@.
--
-- * Each cell of a ship is presented as the ship's first letter:
-- in lowercase, if that cell's not been hit; and
-- in uppercase, if that cell has been hit.

boardToStrs (Board rows) = MatrixUntrusted.matrixToStrs size size rows
