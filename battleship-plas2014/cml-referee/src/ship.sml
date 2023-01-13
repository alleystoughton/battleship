(* ships *)

structure Ship :> SHIP =
struct

datatype ship =
           Carrier
         | Battleship
         | Submarine
         | Destroyer
         | PatrolBoat

fun size Carrier    = 5
  | size Battleship = 4
  | size Submarine  = 3
  | size Destroyer  = 3
  | size PatrolBoat = 2

fun shipToStr Carrier    = "carrier"
  | shipToStr Battleship = "battleship"
  | shipToStr Submarine  = "submarine"
  | shipToStr Destroyer  = "destroyer"
  | shipToStr PatrolBoat = "patrol boat"

fun shipToLetter ship = String.sub(shipToStr ship, 0)

val ships : ship list =
      [Carrier, Battleship, Submarine, Destroyer, PatrolBoat]

fun ord ship =
      let fun od(i, ships) =
                if hd ships = ship
                then i
                else od(i + 1, tl ships)
      in od(0, ships) end

end;
