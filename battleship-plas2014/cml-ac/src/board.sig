(* placing-phase and locked boards *)

(* Trusted module implementing placing-phase and locked boards.

   There are two types of placing-phase boards: placing boards, on
   which some ships have been properly placed, and complete placing
   boards, on which all ships have been properly placed.

   Keyed and locked boards come in pairs.  A cell of the keyed board
   contains a key for unlocking the corresponding cell of the locked
   board iff the keyed board's cell hasn't been shot.  There is a
   one-to-one correspondence between keys and cells---only a cell's
   key will unlock it.  A locked board contains a counter; to shoot a
   cell of a locked board, one needs a counted key whose counter is
   equal to the board's counter, and whose key will unlock the
   cell. *)

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

(* placings -- board on which each ship has been placed at most once,
   but where there is no information about shooting *)

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

(* complete placings *)

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

(* locked boards

   keyed and locked boards are abstract, immutable values; they have
   the same dimensions as game boards

   they come in pairs: a cell of the keyed board contains a key for
   unlocking the corresponding cell of the locked board, assuming that
   cell isn't already unlocked

   the contents of a cell of the keyed board is a cell's membership --
   whether part of a ship, and, if so, which one -- plus an optional
   key; we say the cell has been *shot* iff the optional key is NONE

   the contents of a cell of the locked board is a cell membership; a
   cell of the locked board is either *locked* or *unlocked*

   a locked board contains a counter; to successfully shoot a cell of
   a locked board, one needs a counted key (an abstract value) whose
   counter is equal to the board's counter, and whose key will unlock
   the cell

   shooting a cell of a keyed board returns the optional key of that
   cell, plus an updated keyed board

     if the cell's optional key is NONE, this means the cell was
     already shot, in which case the updated keyed board is the same
     as the original keyed board

     otherwise, the updated keyed board is the same as the original
     one, except that the cell's optional key is replaced by NONE

   successfully shooting a cell of a locked board doesn't give one the
   cell's membership -- it results in a locked shot result, plus an
   updated locked board:

     if the cell's membership says it isn't part of a ship, the result is
     a miss

     if the cell's membership says it's part of ship, and all the
     other cells of ship are unlocked, the result is that ship has
     been sunk

     if the cell's membership says it's part of ship, but not all the
     other cells of ship are unlocked, the result is a hit

   when the shooting of a locked board cell succeeds, that cell of the
   updated locked board is marked as unlocked, and the updated
   locked board's counter is set to be one more than the counter of
   the original board; when the shooting of a cell fails, the
   updated locked board is identical to the original board *)

(* keyed boards *)

type kb

(* locked boards *)

type lb

(* a totally locked board consists of a boolean identity plus a locked
   board in which all cells are locked and the counter is 0 *)

type tlb

(* return the identity of a totally locked board *)

val idOfTLB : tlb -> bool

(* return the locked board of a totally locked board *)

val lbOfTLB : tlb -> lb

(* completeToBoardPair compl creates a board pair (kb, lb), where:

     the memberships of the cells of kb and lb are the corresponding
     memberships of compl;

     all the cells of kb have keys, distinct cells of kb have
     different keys, all the cells of lb are locked, and the key of a
     cell of kb unlocks the corresponding cell of lb; and

     the counter of lb is 0.

   It then returns (kb, tlbFun), where tlbFun takes in an identity id
   and returns the totally locked board with identity id and locked
   board lb *)

val completeToBoardPair : complete -> kb * (bool -> tlb)

(* tests whether all cells of a keyed board that are parts of ships
   have NONE as their optional keys; in other words, tests whether all
   ships of a keyed board have been sunk *)

val keyedAllSunk : kb -> bool

(* tests whether all cells of a locked board that are parts of ships
   have been unlocked/shot; in other words, tests whether all ships of
   a locked board have been sunk *)

val lockedAllSunk : lb -> bool

(* shoot at specified cell of keyed board, resulting in an updated
   keyed board, plus the cell's optional key

   if the cell's optional key is NONE (which means that cell of
   the board was already shot), then the updated keyed board is
   the same as the original one

   otherwise, the updated keyed board is the same as the original
   one except that the cell's optional key is replaced by NONE (meaning
   that this cell of the board has been shot) *)

val keyedShoot : kb * pos -> kb * Key.key option

(* test whether a cell of a locked board has already been
   unlocked/shot *)

val lockedAlreadyShot : lb * pos -> bool

(* counted key -- a key labeled with a nonnegative counter *)

type ck

(* label a key with a counter; raises Domain if the counter isn't
   >= 0 *)

val labelKey : Key.key * int -> ck

(* note that no way of destructing a ck is exported *)

(* result of shooting at locked board -- a locked shot result *)

datatype lsr =
           InvalidLSR            (* invalid counted key for locked board
                                    counter and selected cell *)
         | RepeatLSR             (* illegal repetition *)
         | MissLSR               (* missed a ship *)
         | HitLSR                (* hit an unspecified ship *)
         | SankLSR of Ship.ship  (* sank the given ship *)

(* shoot at specified cell of locked board using counted key, resulting
   in an updated locked board plus a locked shot result

   if the locked board's counter isn't equal to the counted key's counter,
   then the locked board is returned unchanged, along with InvalidLSR

   otherwise, if the selected cell has already been unlocked/shot,
   then the locked board is returned unchanged, along with RepeatLSR

   otherwise, if the key of the counted key won't unlock the selected
   cell, then the locked board is returned unchanged, along with
   InvalidLSR

   otherwise, the locked board is updated to take account of the
   cell's shooting (the cell is marked as unlocked, and the board's
   counter is incremented by 1), and this updated board is returned
   along with a locked shot result explaining what happened in the
   shooting (MissLSR when the cell isn't part of a ship; HitLSR when
   the cell is part of a ship, but at least one other cell of the ship
   is locked; SankLSR ship when the cell is part of ship, ship, and
   all other cells of ship are unlocked) *)

val lockedShoot : lb * pos * ck -> lb * lsr

end;
