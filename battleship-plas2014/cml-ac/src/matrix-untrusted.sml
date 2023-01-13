(* untrusted supplement to small matrices module *)

structure MatrixUntrusted :> MATRIX_UNTRUSTED =
struct

fun coorToLetter i = chr(ord #"a" + i)

fun matrixToStr (cellToChar, ht, wid) rows =
      if Matrix.valid(ht, wid, rows)
      then let fun horizDiv i =
                     if i = wid - 1
                     then "--|"
                     else "-" ^ "-+-" ^ horizDiv(i + 1)

               val horizDiv = "--+-" ^ horizDiv 0

               fun colCoors i =
                     if i = wid - 1
                     then str(coorToLetter i) ^ " |"
                     else str(coorToLetter i) ^ " | " ^ colCoors(i + 1)

               val colCoors = "  | " ^ colCoors 0

               fun rowToStr [cell]          = str(cellToChar cell) ^ " |"
                 | rowToStr (cell :: cells) =
                     str(cellToChar cell) ^ " | " ^ rowToStr cells
                 | rowToStr _               = raise Fail "cannot happen"

               fun rowsToStr i =
                     if i = ht - 1
                     then str(coorToLetter i) ^ " | " ^
                          rowToStr(List.nth(rows, i)) ^ "\n" ^
                          horizDiv
                     else str(coorToLetter i) ^ " | " ^
                          rowToStr(List.nth(rows, i)) ^ "\n" ^
                          horizDiv ^ "\n" ^ rowsToStr(i + 1)
           in colCoors ^ "\n" ^ horizDiv ^ "\n" ^ rowsToStr 0 end
      else raise Matrix.Invalid

fun matrixToStrs (cellToChar, ht, wid) rows =
      String.tokens
      (fn c => c = #"\n")
      (matrixToStr (cellToChar, ht, wid) rows)

end;
