(* game boards *)

structure Board :> BOARD =
struct

val size : int = 10

type pos = int * int

fun swap((i, j) : pos) = (j, i)

fun posToPair(pos : pos) = pos

fun pairToPos((i , j) : int * int) =
      if i >= 0 andalso i < size andalso
         j >= 0 andalso j < size
      then SOME(i, j)
      else NONE

fun letterToCoor c = ord c - ord #"a"

fun coorToLetter n = chr(ord #"a" + n)

fun posToStr(i, j) = implode[coorToLetter i, coorToLetter j]

fun strToPos s =
      case explode s of
           [x, y] =>
             if Char.isLower x andalso Char.isLower y
             then let val i = letterToCoor x
                      val j = letterToCoor y
                  in if i < size andalso j < size
                     then SOME(i, j)
                     else NONE
                  end
             else NONE
         | _      => NONE

datatype orient = Horiz | Vert

fun orientToStr Horiz = "h"
  | orientToStr Vert  = "v"

fun strToOrient "h" = SOME Horiz
  | strToOrient "v" = SOME Vert
  | strToOrient _   = NONE

(* in the signature BOARD, membership comes later, just
   before placingToMatrix and completeToMatrix *)

datatype membership = Part of Ship.ship | NotPart

fun notPart NotPart = true
  | notPart _       = false

type placing = membership Matrix.matrix

val empty = Matrix.const(size, size, NotPart)

fun hasShip rows ship =
      List.exists
      (fn row =>
            List.exists
            (fn (Part ship') => ship' = ship
              | _            => false)
            row)
      rows

fun isComplete rows = List.all (hasShip rows) Ship.ships

datatype 'a place_result =
           PlacingSuccess of 'a
         | DuplicateShip
         | OffBoard
         | OverlapsShip

fun placeRow(us : membership list, ship, j) =
      let val len = Ship.size ship
      in if j + len > size
         then OffBoard
         else let val xs = List.take(us, j)
                  val ys = List.drop(us, j)
                  val zs = List.take(ys, len)
                  val ws = List.drop(ys, len)
              in if List.all notPart zs
                 then PlacingSuccess
                      (xs @
                       List.tabulate(len, fn _ => Part ship) @
                       ws)
                 else OverlapsShip
              end
      end

fun placeHoriz(rows, ship, (i, j)) =
      if hasShip rows ship
      then DuplicateShip
      else let val row = List.nth(rows, i)
           in case placeRow(row, ship, j) of
                   PlacingSuccess row =>
                     PlacingSuccess(Aux.update(rows, i, row))
                 | DuplicateShip      => raise Fail "cannot happen"
                 | OffBoard           => OffBoard
                 | OverlapsShip       => OverlapsShip
           end

fun place(rows, ship, pos, Horiz) = placeHoriz(rows, ship, pos)
  | place(rows, ship, pos, Vert)  =
      case placeHoriz(Matrix.transpose(size, size, rows), ship, swap pos) of
           PlacingSuccess rows =>
             PlacingSuccess(Matrix.transpose(size, size, rows))
         | DuplicateShip       => DuplicateShip
         | OffBoard            => OffBoard
         | OverlapsShip        => OverlapsShip

type complete = placing

fun placingToComplete brd = if isComplete brd then SOME brd else NONE

fun placingToMatrix brd = brd

val completeToMatrix = placingToMatrix

(* a board cell consists of a membership, plus a boolean saying whether
   the cell has been shot (true = shot, false = not shot) *)

type cell = membership * bool

type board = cell Matrix.matrix

val completeToBoard : complete -> board = Matrix.map(fn memb => (memb, false))

fun sunk rows ship =
      foldl
      (fn (row, n) =>
            foldl
            (fn ((Part ship', true), m) =>
                  if ship' = ship then m + 1 else m
              | (_,                  m) => m)
            n
            row)
      0
      rows =
      Ship.size ship

fun allSunk brd = List.all (sunk brd) Ship.ships

datatype shot_result =
           Repeat
         | Miss
         | Hit of Ship.ship
         | Sank of Ship.ship

fun shootRow(cells, j) =
      case List.nth(cells, j) of
           (_,         true)  => (cells, Repeat)
         | (NotPart,   false) =>
             (Aux.update(cells, j, (NotPart, true)), Miss)
         | (Part ship, false) =>
             (Aux.update(cells, j, (Part ship, true)), Hit ship)

fun shoot(rows, (i, j)) =
      let val row       = List.nth(rows, i)
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

fun boardToMatrix brd = brd

end;
