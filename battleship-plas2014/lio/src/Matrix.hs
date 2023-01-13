{-# LANGUAGE Safe #-}

{-|

Module      : Matrix
Description : (Trusted) Small Matrices -- Used to Represent Game Boards.

This trusted module implements the small matrices that are used to
represent game boards.  Matrices are indexed by lowercase letters.

-}

module Matrix
       (Matrix,
        valid,
        Matrix.const,
        sub,
        update,
        Matrix.map,
        Matrix.mapM,
        mapPos,
        transpose)
       where

import qualified Control.Monad as Monad
import qualified Data.List as List

import qualified Aux

-- | A matrix is just a list of lists.

type Matrix a = [[a]]

valid :: Int -> Int -> Matrix a -> Bool

-- ^ @'valid' ht wid rows@ tests whether @ht@ (height) and @wid@ (width) are
-- both at least @1@ and no more than @26@, and that @rows@ is a list of @ht@
-- lists, each of length @wid@.
-- We say that a value @rows@ of type @'Matrix' a@ is /valid/ iff @'valid' ht
-- wid rows@ for some @ht@ and @wid@.

valid ht wid rows =
  ht >= 1 && wid >= 1                   &&
  ht <= 26 && wid <= 26                 &&
  length rows == ht                     &&
  all (\ row -> length row == wid) rows

const :: Int -> Int -> a -> Matrix a

-- ^@'const' ht wid x@ returns a list of @ht@ rows, each consisting
-- of a length-@wid@ list all of whose elements are @x@; it raises
-- an exception if @ht@ and/or @wid@ are outside of the range @1@ .. @26@.

const ht wid x =
  if ht >= 1 && wid >= 1   &&
     ht <= 26 && wid <= 26
  then let row = replicate wid x
       in replicate ht row
  else error "invalid dimension"

sub :: Int -> Int -> Matrix a -> Int -> Int -> a

-- ^ @'sub' ht wid rows i j@ raises an exception if @'valid' ht wid rows@
-- is 'False' or @i \< 0@ or @i \>= ht@ or @j \< 0@ or @j \>= wid@; otherwise,
-- it returns the @j@th element of the @i@th element of @rows@.

sub ht wid rows i j =
  if valid ht wid rows &&
     0 <= i && i < ht &&
     0 <= j && j < wid
  then (rows !! i) !! j
  else error "invalid dimension or position"

update :: Int -> Int -> Matrix a -> Int -> Int -> a -> Matrix a

-- ^ @'update' ht wid rows i j x@ raises an exception if @'valid' ht
-- wid rows@ is 'False' or @i \< 0@ or @i \>= h@t or @j \< 0@ or @j \>=
-- wid@; otherwise, it returns the matrix that's the same as @rows@
-- except that the @j@th element of the @i@th element of this matrix
-- is @x@.

update ht wid rows i j x =
  if valid ht wid rows &&
     0 <= i && i < ht &&
     0 <= j && j < wid
  then Aux.update rows i (Aux.update (rows !! i) j x)
  else error "invalid dimension or position"

map :: (a -> b) -> Matrix a -> Matrix b

-- ^ @'map' f rows@ transforms @rows@ by applying @f@ to each element
-- of each of its elements.

map f = List.map(List.map f)

mapM :: Monad m => (a -> m b) -> Matrix a -> m(Matrix b)

-- ^ Monadic version of 'Matrix.map': works through the rows from top to bottom,
-- and through each row's cells from left to right.

mapM f rows =
  Monad.mapM
  (\ row ->
    Monad.mapM
    (\ cell -> f cell)
    row)
  rows

mapPos :: (Int -> Int -> a -> b) -> Matrix a -> Matrix b

-- ^ Like 'Matrix.map', but the function @f@ to be mapped over the
-- matrix is also called with initial arguments @i@ and @j@,
-- consisting of the row and column, respectively, of the cell to be
-- transformed.

mapPos f rows =
  Aux.mapInd
  (\ i row ->
    Aux.mapInd
    (\ j cell -> f i j cell)
    row)
  rows

transpose :: Int -> Int -> Matrix a -> Matrix a

-- ^ @'transpose' ht wid rows@ raises an exception if @'valid' ht wid rows@ is
-- 'False'; otherwise, it transposes @rows@ (swapping rows and columns);
-- the result, @cols@, will satisfy @'valid' wid ht cols@.

transpose ht wid rows =
  if valid ht wid rows
  then List.transpose rows
  else error "invalid matrix"
