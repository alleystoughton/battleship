(* untrusted supplement to board module *)

(* Untrusted module implementing a supplement to the board module.  It
   defines an abstract type of shooting-phase boards, and defines
   functions for describing boards as list of strings. *)

signature BOARD_UNTRUSTED =
sig

(* shooting-phase boards -- consisting of placed ships plus shooting
   record *)

type board

(* turns a complete placing into a board on which no shooting has
   occurred *)

val completeToBoard : Board.complete -> board

(* result of shooting *)

datatype shot_result =
           Repeat             (* an attempt to shoot a position already shot *)
         | Miss               (* miss -- no ship hit *)
         | Hit of Ship.ship   (* hit a ship *)
         | Sank of Ship.ship  (* sank a ship -- last of its cells hit *)

(* shoot a given position of a board; the returned shot_result
   indicates the result of the shooting *)

val shoot : board * Board.pos -> board * shot_result

(* convert a placing to a list of strings describing it

   see matrix-untrusted.sig for the general format; each cell of a
   ship is presented as the ship's first letter, in lowercase; vacant
   cells are presented as " " *)

val placingToStrs : Board.placing -> string list

(* convert a complete placing to a list of strings describing it --
   works identically to placingToStrs *)

val completeToStrs : Board.complete -> string list

(* convert a board to a list of strings describing it

   see matrix-untrusted.sig for the general format

   each vacant cell (not part of a ship) that's not been shot at is
   presented as " "

   each vacant cell that's been shot at is labeled "*"

   each cell of a ship is presented as the ship's first letter:

     - in lowercase, if that cell's not been hit
     - in uppercase, if that cell has been hit *)

val boardToStrs : board -> string list

end;
