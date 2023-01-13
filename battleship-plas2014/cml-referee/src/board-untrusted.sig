(* untrusted supplement to board module *)

(* Untrusted module implementing a supplement to the board module.  It
   defines functions for converting boards to lists of strings
   describing them. *)

signature BOARD_UNTRUSTED =
sig

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

val boardToStrs : Board.board -> string list

end;
