(* untrusted supplement to board module *)

structure BoardUntrusted :> BOARD_UNTRUSTED =
struct

fun membershipToChar (Board.Part ship) = Ship.shipToLetter ship
  | membershipToChar Board.NotPart     = #" "

fun placingToStrs plac =
      MatrixUntrusted.matrixToStrs
      (membershipToChar, Board.size, Board.size)
      (Board.placingToMatrix plac)

fun completeToStrs plac =
      MatrixUntrusted.matrixToStrs
      (membershipToChar, Board.size, Board.size)
      (Board.completeToMatrix plac)

fun cellToChar(Board.Part ship, false) = Ship.shipToLetter ship
  | cellToChar(Board.Part ship, true)  = Char.toUpper(Ship.shipToLetter ship)
  | cellToChar(Board.NotPart,   false) = #" "
  | cellToChar(Board.NotPart,   true)  = #"*"

fun boardToStrs brd =
      MatrixUntrusted.matrixToStrs
      (cellToChar, Board.size, Board.size)
      (Board.boardToMatrix brd)

end;
