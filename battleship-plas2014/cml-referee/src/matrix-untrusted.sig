(* untrusted supplement to small matrices module *)

(* Untrusted module implementing a supplement to the small matrices
   module. *)

signature MATRIX_UNTRUSTED =
sig

(* matrixToStrs (cellToChar, ht, wid) rows raises Matrix.Invalid if
   Matrix.valid(ht, wid, rows) is false; otherwise it converts rows
   into a list of strings that describes it, using cellToChar to
   convert each cell of the matrix to a character, and indexing the
   rows and columns by lowercase letters (a, b, c, ...)

   e.g.,

     fun f true  = #"1"
       | f false = #"0";
     val xs =
           matrixToStrs
           (f, 2, 3)
           [[true, false, true], [false, true, true]];
     app (fn s => print(s ^ "\n")) xs;

   results in

       | a | b | c |
     --+---+---+---|
     a | 1 | 0 | 1 |
     --+---+---+---|
     b | 0 | 1 | 1 |
     --+---+---+---|
*)

val matrixToStrs : ('a -> char) * int * int -> 'a Matrix.matrix -> string list

end;
