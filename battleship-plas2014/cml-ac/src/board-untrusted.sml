(* untrusted supplement to board module *)

structure BoardUntrusted :> BOARD_UNTRUSTED =
struct

(* a board cell consists of a membership plus a boolean saying whether
   cell has been shot (true = shot, false = not shot) *)

type cell = Board.membership * bool

type board = cell Matrix.matrix

fun completeToBoard compl =
      Matrix.map
      (fn memb => (memb, false))
      (Board.completeToMatrix compl)

fun sunk rows ship =
      foldl
      (fn (row, n) =>
            foldl
            (fn ((Board.Part ship', true), m) =>
                  if ship' = ship then m + 1 else m
              | (_,                        m) => m)
            n
            row)
      0
      rows =
      Ship.size ship

datatype shot_result =
           Repeat
         | Miss
         | Hit of Ship.ship
         | Sank of Ship.ship

fun shootRow(cells, j) =
      case List.nth(cells, j) of
           (_,               true)  => (cells, Repeat)
         | (Board.NotPart,   false) =>
             (Aux.update(cells, j, (Board.NotPart, true)), Miss)
         | (Board.Part ship, false) =>
             (Aux.update(cells, j, (Board.Part ship, true)), Hit ship)

fun shoot(rows, pos) =
      let val (i, j)    = Board.posToPair pos
          val row       = List.nth(rows, i)
          val (row, sh) = shootRow(row, j)
          val rows      = Aux.update(rows, i, row)
          val sh        =
                case sh of
                     Repeat   => Repeat
                   | Miss     => Miss
                   | Hit ship =>
                       if sunk rows ship
                       then Sank ship
                       else Hit ship
                   | Sank _   => raise Fail "cannot happen"
      in (rows, sh) end

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

val boardToStrs =
      MatrixUntrusted.matrixToStrs(cellToChar, Board.size, Board.size)

end;
