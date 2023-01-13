(* small matrices -- used to represent game boards *)

(* Trusted module implementing the small matrices used to represent
   game boards. *)

signature MATRIX =
sig

(* exception raised to indicate an error *)

exception Invalid

type 'a matrix = 'a list list

(* valid(ht, wid, rows) tests whether ht (height) and wid (width) are
   both at least 1 and no more than 26, and that rows is a list of ht
   lists, each of length wid

   we say that a value rows of type 'a matrix is *valid* iff
   valid(ht, wid, rows) for some ht and wid *)

val valid : int * int * 'a matrix -> bool

(* const(ht, wid, x) returns a list of ht rows, each consisting
   of a length-wid list all of whose elements are x; it raises
   Invalid if ht and/or wid are outside of the range 1 .. 26 *)

val const : int * int * 'a -> 'a matrix

(* sub(ht, wid, rows, i, j) raises Invalid if valid(ht, wid, rows)
   is false or i < 0 or i >= ht or j < 0 or j >= wid; otherwise,
   it returns the jth element of the ith element of rows *)

val sub : int * int * 'a matrix * int * int -> 'a

(* update(ht, wid, rows, i, j, x) raises Invalid if valid(ht, wid,
   rows) is false or i < 0 or i >= ht or j < 0 or j >= wid; otherwise,
   it returns the matrix that's the same as rows except that the jth
   element of the ith element of this matrix is x *)

val update : int * int * 'a matrix * int * int * 'a -> 'a matrix

(* map f rows transforms rows by applying f to each element
   of each of its elements *)

val map : ('a -> 'b) -> 'a matrix -> 'b matrix

(* transpose(ht, wid, rows) raises Invalid if valid(ht, wid, rows) is
   false; otherwise, it transposes rows (swapping rows and columns);
   the result, cols, will satisfy valid(wid, ht, cols) *)

val transpose : int * int * 'a matrix -> 'a matrix

end;
