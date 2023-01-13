(* auxiliary functions *)

structure Aux :> AUX =
struct

fun fatal(comp, msg) =
  (print("fatal error: " ^ comp ^ ": " ^ msg ^ "\n");
   RunCML.shutdown OS.Process.failure)

fun catch (comp, msg) f x =
      f x
        handle _ =>
          fatal(comp, msg)

fun update(xs, i, y) =
      let fun loop(_,  _, nil)     = raise Subscript
            | loop(us, 0, _ :: vs) = List.revAppend(us, y :: vs)
            | loop(us, i, v :: vs) = loop(v :: us, i - 1, vs)
      in if i < 0
         then raise Subscript
         else loop(nil, i, xs)
      end

fun strToInt x =
      (case Int.scan StringCvt.DEC Substring.getc (Substring.full x) of
            NONE       => NONE
          | SOME(n, x) =>
              let val x = StringCvt.skipWS Substring.getc x
              in if Substring.isEmpty x
                 then SOME n
                 else NONE
              end)
        handle _ => NONE

fun dropWhile f nil             = nil
  | dropWhile f (ys as x :: xs) =
      if f x
      then dropWhile f xs
      else ys

fun strip x =
      let val cs = explode x
          val ds = dropWhile Char.isSpace cs
          val es = rev(dropWhile Char.isSpace (rev ds))
      in implode es end

fun allButLast nil       = raise Empty
  | allButLast [_]       = nil
  | allButLast (x :: xs) = x :: allButLast xs

fun fromTo(i, j) =
      if i > j
      then nil
      else i :: fromTo(i + 1, j)

fun insert cmp (x, nil)           = [x]
  | insert cmp (x, zs as y :: ys) =
      case cmp (x, y) of
           LESS    => x :: zs
         | EQUAL   => zs
         | GREATER => y :: insert cmp (x, ys)

fun sort _   nil       = nil
  | sort cmp (x :: xs) = insert cmp (x, sort cmp xs)

end;
