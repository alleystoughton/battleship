(* placing-phase and locked boards *)

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
                 | DuplicateShip     => raise Fail "cannot happen"
                 | OffBoard          => OffBoard
                 | OverlapsShip      => OverlapsShip
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

(* locked boards *)

(* keyed or locked cells; a cell is labeled NONE iff it has already
   been unlocked/shot *)

type k_l_cell = membership * Key.key option

type k_l_board = k_l_cell Matrix.matrix

type kb = k_l_board

type lb = k_l_board * int  (* counter >= 0 *)

type tlb = bool * lb

fun idOfTLB(id, _) = id

fun lbOfTLB(_, brd) = brd

fun completeToBoardPair compl =
      let fun membershipToCell memb =
                let val key = Key.newKey()
                in (memb, SOME key) end

          val kb = Matrix.map membershipToCell compl
          val lb = (kb, 0)
      in (kb, fn id => (id, lb)) end

fun klSunk rows ship =
      foldl
      (fn (row, n) =>
            foldl
            (fn ((Part ship', NONE), m) =>
                  if ship' = ship then m + 1 else m
              | (_,                  m) => m)
            n
            row)
      0
      rows =
      Ship.size ship

fun allKLSunk brd = List.all (klSunk brd) Ship.ships

val keyedAllSunk = allKLSunk

fun lockedAllSunk(brd, _) = allKLSunk brd

fun keyedShootRow(cells, j) =
      case List.nth(cells, j) of
           (_,         NONE)     => (cells, NONE)
         | (NotPart,   SOME key) =>
             (Aux.update(cells, j, (NotPart, NONE)), SOME key)
         | (Part ship, SOME key) =>
             (Aux.update(cells, j, (Part ship, NONE)), SOME key)

fun keyedShoot(rows, (i, j)) =
      let val row           = List.nth(rows, i)
          val (row, keyOpt) = keyedShootRow(row, j)
          val rows          = Aux.update(rows, i, row)
      in (rows, keyOpt) end

fun lockedAlreadyShot((rows, _), (i, j)) =
      case List.nth(List.nth(rows, i), j) of
           (_, NONE)   => true
         | (_, SOME _) => false

(* counted key *)

type ck = Key.key * int

fun labelKey(key, ctr) =
      if ctr >= 0
      then (key, ctr)
      else raise Domain 

datatype lsr =
           InvalidLSR
         | RepeatLSR
         | MissLSR
         | HitLSR
         | SankLSR of Ship.ship

datatype lsr' =  (* used by lockedShootRow *)
           InvalidLSR'
         | RepeatLSR'
         | MissLSR'
         | HitLSR' of Ship.ship  (* note: hit ship specified *)

fun lockedShootRow(cells, j, key) =
      case List.nth(cells, j) of
           (_,         NONE)      => (cells, RepeatLSR')
         | (NotPart,   SOME key') =>
             if Key.sameKey(key, key')
             then (Aux.update(cells, j, (NotPart, NONE)), MissLSR')
             else (cells, InvalidLSR')
         | (Part ship, SOME key') =>
             if Key.sameKey(key, key')
             then (Aux.update(cells, j, (Part ship, NONE)), HitLSR' ship)
             else (cells, InvalidLSR')

fun lockedShoot(brd as (rows, ctr), (i, j), ck : ck) =
      if ctr <> #2 ck
      then (brd, InvalidLSR)
      else let val row       = List.nth(rows, i)
               val (row, sh) = lockedShootRow(row, j, #1 ck)
               val rows      = Aux.update(rows, i, row)
               val sh        =
                     case sh of
                          InvalidLSR'  => InvalidLSR
                        | RepeatLSR'   => RepeatLSR
                        | MissLSR'     => MissLSR
                        | HitLSR' ship =>
                            if klSunk rows ship
                            then SankLSR ship
                            else HitLSR
           in case sh of
                   InvalidLSR => ((rows, ctr), sh)
                 | RepeatLSR  => ((rows, ctr), sh)
                 | _          => ((rows, ctr + 1), sh)
           end

end;
