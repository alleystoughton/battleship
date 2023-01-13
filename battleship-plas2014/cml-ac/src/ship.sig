(* ships *)

(* Trusted module, implementing the datatype of ships and associated
   functions. *)

signature SHIP =
sig

(* the ships *)

datatype ship =
           Carrier
         | Battleship
         | Submarine
         | Destroyer
         | PatrolBoat

(* ship sizes

     size Carrier    = 5
     size Battleship = 4
     size Submarine  = 3
     size Destroyer  = 3
     size PatrolBoat = 2 *)

val size : ship -> int

(* convert a ship to a string

   first letters must be in lowercase and be distinct

     shipToStr Carrier    = "carrier"
     shipToStr Battleship = "battleship"
     shipToStr Submarine  = "submarine"
     shipToStr Destroyer  = "destroyer"
     shipToStr PatrolBoat = "patrol boat" *)

val shipToStr : ship -> string

(* the first letter of the string corresponding to a ship *)

val shipToLetter : ship -> char

(* the list of all ships, listed without duplicates, in the order of
   the datatype *)

val ships : ship list

(* index (starting from 0) of ship in list of ships *)

val ord : ship -> int

end;
