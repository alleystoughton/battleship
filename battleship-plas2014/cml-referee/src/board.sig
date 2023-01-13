(* game boards *)

(* Trusted module implementing three types of game boards: placing
   boards, on which some ships have been properly placed; complete
   placing boards, on which all ships have been properly placed; and
   the boards of the shooting phase. *)

signature BOARD =
sig

(* board size: 10

   must be >= 1 and <= 26; for the game to be playable, must be big
   enough to accomodate all the ships of SHIP *)

val size : int

(* a position on a board -- represents a pair (i, j) of coordinates,
   where i, j are in the range 0 .. size - 1, i indexes a row, and j
   indexes a column *)

(* a board position *)

type pos

(* converts a position to the pair it represents *)

val posToPair : pos -> int * int

(* converts a pair to SOME of the position that represents it,
   returning NONE when the pair isn't represented by a position *)

val pairToPos : int * int -> pos option

(* converts a position representing (i, j) into the string
   RC, where R is the ith lowercase letter (counting from 0),
   and C is the jth lowercase letter *)

val posToStr : pos -> string

(* converts a string to SOME of the position it describes,
   returning NONE if it doesn't describe a position *)

val strToPos : string -> pos option

(* a ship orientation *)

datatype orient = Horiz | Vert

(* converts an orientation to a string: Horiz goes to "h"
   and Vert goes to "v" *)

val orientToStr : orient -> string

(* converts a string to SOME of the orientation it describes,
   returning NONE when the string doesn't describe an orientation *)

val strToOrient : string -> orient option

(* a board on which each ship has been placed at most once, but
   where there is no information about shooting *)

type placing

(* a placing with no ships *)

val empty : placing

(* tests whether a placing is *complete*, i.e., whether all ships have
   been placed on the placing *)

val isComplete : placing -> bool

(* result of placing a ship *)

datatype 'a place_result =
           PlacingSuccess of 'a  (* successful placing *)
         | DuplicateShip         (* ship was already placed *)
         | OffBoard              (* ship (partially) off the board *)
         | OverlapsShip          (* ship overlaps another ship *)

(* attempt to place a ship on a placing at a given position and with a
   given orientation; when successful, returns PlacingSuccess of the
   resulting placing; otherwise, the constructor indicates why the
   placing failed *)

val place : placing * Ship.ship * pos * orient -> placing place_result

(* a complete placing *)

type complete

(* returns NONE if its argument isn't complete; otherwise
   returns SOME of its argument *)

val placingToComplete : placing -> complete option

(* membership of a board cell -- whether part of a ship, and, if so,
   which one *)

datatype membership = Part of Ship.ship | NotPart

(* export a placing to its membership matrix *)

val placingToMatrix : placing -> membership Matrix.matrix

(* export a complete placing to its membership matrix *)

val completeToMatrix : complete -> membership Matrix.matrix

(* a shooting-phase board consisting of placed ships plus shooting
   record *)

type board

(* turns a complete placing into a board on which no shooting has
   occurred *)

val completeToBoard : complete -> board

(* tests whether all ships have been sunk, i.e., all cells
   of all ships have been shot *)

val allSunk : board -> bool

(* result of shooting *)

datatype shot_result =
           Repeat             (* an attempt to shoot a position already shot *)
         | Miss               (* miss -- no ship hit *)
         | Hit of Ship.ship   (* hit a ship *)
         | Sank of Ship.ship  (* sank a ship -- last of its cells hit *)

(* shoot a given position of a board; the returned shot_result
   indicates the result of the shooting *)

val shoot : board * pos -> board * shot_result

(* cell of shooting-phase board *)

type cell = membership * bool

(* export a board to its cell matrix *)

val boardToMatrix : board -> cell Matrix.matrix

end;
