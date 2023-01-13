{-# LANGUAGE Safe #-}

{-|

Module      : Ship
Description : (Trusted) Ships Datatype, and Associated Functions.

This trusted module defines the datatype of ships, along with associated
functions.

-}

module Ship
       (Ship(..),
        size,
        shipToStr,
        shipToLetter,
        ships,
        ord
       ) where  

-- | The ships.

data Ship =
  -- | The carrier.
  Carrier
  -- | The battleship.
  | Battleship
    -- | The submarine.
  | Submarine
    -- | The destroyer.
  | Destroyer
    -- | The patrol boat.
  | PatrolBoat
  deriving (Eq, Ord, Enum, Bounded)

size :: Ship -> Int

-- ^ Number of cells in a ship:
--
-- @
--   'size' 'Carrier'    = 5
--   'size' 'Battleship' = 4
--   'size' 'Submarine'  = 3
--   'size' 'Destroyer'  = 3
--   'size' 'PatrolBoat' = 2
-- @

size Carrier    = 5
size Battleship = 4
size Submarine  = 3
size Destroyer  = 3
size PatrolBoat = 2

shipToStr :: Ship -> String

-- ^ Description of a ship (first letters must be distinct and in
-- lowercase):
--
-- @
--   'shipToStr' 'Carrier'    = \"carrier\"
--   'shipToStr' 'Battleship' = \"battleship\"
--   'shipToStr' 'Submarine'  = \"submarine\"
--   'shipToStr' 'Destroyer'  = \"destroyer\"
--   'shipToStr' 'PatrolBoat' = \"patrol boat\"
-- @

shipToStr Carrier    = "carrier"
shipToStr Battleship = "battleship"
shipToStr Submarine  = "submarine"
shipToStr Destroyer  = "destroyer"
shipToStr PatrolBoat = "patrol boat"

shipToLetter :: Ship -> Char

-- ^ First letter of ship's description.

shipToLetter ship = head $ shipToStr ship

ships :: [Ship]

-- ^ List of ships, in order given by datatype.

ships = [minBound .. maxBound]

ord :: Ship -> Int

-- ^ Index (starting from @0@) of ship in list of ships.

ord ship = fromEnum ship
