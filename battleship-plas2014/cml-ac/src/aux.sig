(* auxiliary functions *)

(* Trusted module implementing several auxiliary functions. *)

signature AUX =
sig

(* fatal(comp, msg) issues the error message msg, attributed to
   program component comp, on the standard output, and then causes the
   program to exit with failure status *)

val fatal : string * string -> 'a

(* catch (comp, msg) f x runs f x

   if f x terminates normally with a value, then catch returns this
   value

   if f x raises an exception, then catch issues the error message
   msg, attributed to program component comp, on the standard output,
   and then causes the program to exit with failure status *)

val catch : string * string -> ('a -> 'b) -> 'a -> 'b

(* update(xs, i, y) returns the list that's like xs except that its
   ith (counting from 0) element is y; raises Subscript if i isn't an
   index into xs *)

val update : 'a list * int * 'a -> 'a list

(* strToInt x returns NONE, if either x isn't a numeral, or x is a
   numeral whose corresponding integer is outside the range of type
   int; otherwise, it returns SOME of the integer corresponding to x

   numerals are in base-10, and negative numerals have a leading
   "~" or "-" *)

val strToInt : string -> int option

(* strip x removes leading and trailing whitespace from x *)

val strip : string -> string

(* allButLast xs returns all the elements of xs except the final one;
   raises Empty when xs is empty *)

val allButLast : 'a list -> 'a list

(* fromTo(i, j) returns the list of integers between i and j, inclusive,
   listed in strictly ascending order *)

val fromTo : int * int -> int list

(* sort cmp xs sorts xs using the total ordering cmp, removing duplicates *)

val sort : ('a * 'a -> order) -> 'a list -> 'a list

end;
