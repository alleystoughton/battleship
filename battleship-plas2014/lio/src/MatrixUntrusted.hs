{-# LANGUAGE Safe #-}

{-|

Module      : MatrixUntrusted
Description : (Untrusted) Supplement to Small Matrices Module.

This untrusted module supplements the small matrices module with
functions for describing boards as strings and lists of strings.

-}

module MatrixUntrusted
       (Cell(..),
        matrixToStr,
        matrixToStrs)
       where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Char as Char

import qualified Aux
import Matrix

-- | Typeclass for matrix cells: provides a function for converting
-- a cell to a character.

class Cell a where

  -- | Convert an instance of class to character.

  toChar :: a -> Char

-- convert a matrix to a list of strings describing it

coorToLetter :: Int -> Char

coorToLetter i = Char.chr(Char.ord 'a' + i)

matrixToStr :: Cell a => Int -> Int -> Matrix a -> String

-- ^ @'matrixToStr' ht wid rows@ raises an exception if @'valid' ht
-- wid rows@ is 'False'.  Otherwise it converts @rows@ into a string
-- that describes it, using 'toChar' (from typeclass 'Cell') to
-- convert each cell of the matrix to a character, and indexing the
-- rows and columns by lowercase letters (@a@, @b@, @c@, ...).
--
-- E.g., if
--
-- @
--   instance 'Cell' 'Bool' where
--     'toChar' 'True'  = '1'
--     'toChar' 'False' = '0'
-- @
--
-- then
--
-- @
--   'matrixToStr' 2 3 [[true, false, true], [false, true, true]]
-- @
--
-- produces a string that prints as
--
-- @
--     | a | b | c |
--   --+---+---+---|
--   a | 1 | 0 | 1 |
--   --+---+---+---|
--   b | 0 | 1 | 1 |
--   --+---+---+---|
-- @

matrixToStr ht wid rows =
  if valid ht wid rows
  then let horizDiv i =
             if i == wid - 1
             then "--|"
             else "-" ++ "-+-" ++ horizDiv(i + 1)

           horizDiv' = "--+-" ++ horizDiv 0

           colCoors i =
             if i == wid - 1
             then [coorToLetter i] ++ " |"
             else [coorToLetter i] ++ " | " ++ colCoors(i + 1)

           colCoors' = "  | " ++ colCoors 0

           rowToStr cells =
             case cells of
                  [cell]       -> [toChar cell] ++ " |"
                  cell : cells ->
                    [toChar cell] ++ " | " ++ rowToStr cells
                  _            -> error "cannot happen"

           rowsToStr i =
             if i == ht - 1
             then [coorToLetter i] ++ " | " ++ rowToStr(rows !! i) ++
                  "\n" ++ horizDiv'
             else [coorToLetter i] ++ " | " ++ rowToStr(rows !! i) ++
                  "\n" ++ horizDiv' ++ "\n" ++ rowsToStr(i + 1)
       in colCoors' ++ "\n" ++ horizDiv' ++ "\n" ++ rowsToStr 0
  else error "invalid matrix"

matrixToStrs :: (Cell a) => Int -> Int -> Matrix a -> [String]

-- ^ Like 'matrixToStr' except divides the resulting string into lines
-- (not including trailing newlines).

matrixToStrs ht wid rows = List.lines $ matrixToStr ht wid rows
