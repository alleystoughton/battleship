{-# LANGUAGE Safe #-}

{-|

Module      : Aux
Description : (Trusted) Auxiliary Functions.

This trusted module defines several auxiliary functions.

-}

module Aux
       (update,
        strToInteger,
        mapInd,
        tokens,
        strip,
        allButLast)
       where  

import Data.Char
import qualified Data.Text as Text

update :: [a] -> Int -> a -> [a]

-- ^ @'update' xs i y@ returns the list that's like @xs@ except that its @i@th
-- (counting from @0@) element is @y@. It raises an exception if @i@ isn't an
-- index into @xs@.

update xs i y =
  let (us, vs) = splitAt i xs
  in (us ++ [y] ++ tail vs)

isWhitespace :: String -> Bool

isWhitespace = foldr (\ c b -> isSpace c && b) True

strToInteger :: String -> Maybe Integer

-- ^ @'strToInteger' x@ returns 'Nothing', if @x@ isn't a numeral;
-- oherwise, it returns 'Just' of the integer corresponding to @x@.
-- Numerals are in base-10, and negative numerals have a leading
-- @\"-\"@.

strToInteger x =
  case reads x :: [(Integer, String)] of
    [(n, y)] ->
      if isWhitespace y
      then Just n
      else Nothing
    _        -> Nothing

mapInd :: (Int -> a -> b) -> [a] -> [b]

-- ^ @'mapInd' f [x_0, ..., x_{n - 1}]@ returns
-- @[f 0 x_0, ..., f (n - 1) x_{n - 1}]@.

mapInd f xs =
  let mapI _ []       = []
      mapI i (x : xs) = f i x : mapI (i + 1) xs
  in mapI 0 xs

tokens :: Eq a => (a -> Bool) -> [a] -> [[a]]

-- ^ @'tokens' f xs@ returns the unique list @[ys_1, ..., ys_n]@ of nonempty
-- sublists of @xs@ such that there are nonempty lists @zs_1@, ...
-- @zs_{n - 1}@ such that
--
--   * @xs = ys_1 ++ zs_1 ++ ... ++ ys_{n - 1} ++ zs_{n - 1} ++ ys_n@; and
--
--   * @all f zs_1 && ... && all f zs_{n - 1} == True@.

tokens f [] = []
tokens f xs =
  let (ys, zs) = break f xs
  in if null zs
     then [ys]
     else if null ys
          then tokens f (tail zs)
          else ys : tokens f (tail zs)

strip :: String -> String
         
-- ^ @'strip' x@ removes leading and trailing whitespace from @xs@.

strip x = Text.unpack(Text.strip(Text.pack x))

allButLast :: [a] -> [a]

-- ^ @'allButLast' xs@ returns all elements of @xs@ except the final one.
-- Raises an exception when @xs@ is empty.

allButLast [_]      = []
allButLast (x : xs) = x : allButLast xs
