(* shot boards -- tracking shots on opponent's board *)

(* Untrusted module implementing shot boards, which record what has
   been learned about an opponent's board though shooting. *)

signature SHOT_BOARD =
sig

(* a shot board records what has been learned about an opponent's
   board though shooting

   if one starts with the empty shot board, and then uses miss, hit
   and sank to record what is learned through shooting at a sequence
   of distinct positions of the opponent's board, then the shot board
   will remain consistent with the opponent's board

   no attempt is made to detect when a sequence of calls of miss, hit
   and sank doesn't correspond to an actual shooting history -- but
   the resulting shot board will be meaningless in such a case

   a cell of a shot board will either be:

     unshot

     a miss -- shot, but not part of a ship

     an unspecified hit -- shot, and part of an unknown (possibily
     shot) ship

     a specified hit -- shot, and part of a specified, sunk ship

   in addition to the matrix of cells, the shot board records the
   possible locations of sunk ships: for each sunk ship, it is
   guaranteed that one of the recorded possibilities is correct *)

type board

(* a board in which all cells are unshot *)

val empty : board

(* mark a given cell as a miss; this doesn't change the record of
   possible locations of sunk ships *)

val miss : board * Board.pos -> board

(* mark a given cell as an unspecified hit that didn't sink a ship;
   this doesn't change the record of possible locations of sunk ships
   *)

val hit : board * Board.pos -> board

(* indicate that shooting at the specified position sank the
   specified ship

   the specified position of the matrix is recorded as a hit of the
   specified ship

   and the possible locations of the ship are added to the record of
   possible sunk ship locations

   then a loop is run, in which:

     possible sunk ship locations that are inconsistent with the cell
     matrix are discarded

     when only one possible location for a sunk ship remains, the matrix
     is updated so that the ship's cells are all specified hits for that
     ship *)

val sank : board * Board.pos * Ship.ship -> board

(* convert a shot board to a list of strings describing it

   see matrix-untrusted.sig for the general format of the cell matrix:

     an unshot cell is presented as " "

     a miss is presented as "*"

     an unspecified hit is presented as "+"

     a specified hit is presented as the uppercase version of the
     first letter of the ship's name

   the possible locations of sunk ships are described using the
   [a-z][a-z]:[h+v] notation *)

val boardToStrs : board -> string list

end;
