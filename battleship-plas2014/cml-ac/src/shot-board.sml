(* shot boards -- tracking shots on opponent's board *)

structure ShotBoard :> SHOT_BOARD =
struct

(* cell of shot board *)

datatype cell =
           UnShot                   (* not shot *)
         | Miss                     (* shot, but not part of ship *)
         | Hit of Ship.ship option  (* Hit NONE means shot cell of *)
                                    (* unspecified ship; Hit(SOME
                                       ship) means shot cell of ship *)

(* a placing consists of a ship, the position of its leftmost/topmost
   cell, and its orientation *)

type placing = Ship.ship * Board.pos * Board.orient

(* a shot board records what has been learned about an opponent's
   board though shooting

   a shot board consists of a cell matrix, recording the results of
   the shots made so far, plus a list of placings, giving the possible
   locations of the sunk ships *)

type board = cell Matrix.matrix * placing list

val empty : board = (Matrix.const(Board.size, Board.size, UnShot), nil)

fun sub(rows, (i, j)) = Matrix.sub(Board.size, Board.size, rows, i, j)

fun update(rows, (i, j), cell) =
      Matrix.update(Board.size, Board.size, rows, i, j, cell)

fun comparePlacing((ship, pos, orient), (ship', pos', orient')) =
      let val (i, j)   = Board.posToPair pos
          val (i', j') = Board.posToPair pos'
      in
          case Int.compare(Ship.ord ship, Ship.ord ship') of
               LESS    => LESS
             | EQUAL   =>
                 (case (orient, orient') of
                       (Board.Horiz, Board.Vert)  => LESS
                     | (Board.Vert,  Board.Horiz) => GREATER
                     | _                          =>
                        (case Int.compare(i, i') of
                              LESS    => LESS
                            | EQUAL   => Int.compare(j, j')
                            | GREATER => GREATER))
             | GREATER => GREATER
      end

val sortPlacing = Aux.sort comparePlacing

fun placingToStr(ship, pos, orient) =
      Ship.shipToStr ship ^ " could be at " ^
      Board.posToStr pos ^ ":" ^ Board.orientToStr orient

fun placingsToStrs placngs = map placingToStr (sortPlacing placngs)

(* placings(ship, pos) are the placings of ship, both horizontal and
   vertical, that include position pos *)

fun placings(ship, pos) =
      let val siz = Ship.size ship

          fun shipBegins k =
                let val xs = Aux.fromTo(k - siz + 1, k)
                in List.filter
                   (fn n => n >= 0 andalso n + siz - 1 < Board.size)
                   xs
                end

          val (i, j) = Board.posToPair pos
          val horizs =
                map
                (fn k => (ship, valOf(Board.pairToPos(i, k)), Board.Horiz))
                (shipBegins j)
          val verts  =
                map
                (fn k => (ship, valOf(Board.pairToPos(k, j)), Board.Vert))
                (shipBegins i)
      in horizs @ verts end

fun consistent rows (ship, pos, Board.Horiz) =
      let val (i, j) = Board.posToPair pos
          val stop   = j + Ship.size ship

          fun con k =
                k = stop orelse
                (case sub(rows, (i, k)) of
                      Hit NONE        => true
                    | Hit(SOME ship') => ship' = ship
                    | _               => false) andalso
                con(k + 1)
      in con j end
  | consistent rows (ship, pos, Board.Vert) =
      let val (i, j) = Board.posToPair pos
          val stop   = i + Ship.size ship

          fun con k =
                k = stop orelse
                (case sub(rows, (k, j)) of
                      Hit NONE        => true
                    | Hit(SOME ship') => ship' = ship
                    | _               => false) andalso
                con(k + 1)
      in con i end

fun applyPlacing(rows, (ship, pos, Board.Horiz)) =
      let val (i, j) = Board.posToPair pos
          val stop   = j + Ship.size ship

          fun apply(rows, k) =
                if k = stop
                then rows
                else apply(update(rows, (i, k), Hit(SOME ship)), k + 1)
      in apply(rows, j) end
  | applyPlacing(rows, (ship, pos, Board.Vert))  =
      let val (i, j) = Board.posToPair pos
          val stop   = i + Ship.size ship

          fun apply(rows, k) =
                if k = stop
                then rows
                else apply(update(rows, (k, j), Hit(SOME ship)), k + 1)
      in apply(rows, i) end

fun splitPlacingsByShipUniqueness placngs =
      let fun split(uniqs, nonUniqs, nil)                                 =
                (uniqs, nonUniqs)
            | split(uniqs, nonUniqs, (placng as (ship, _, _)) :: placngs) =
                if List.exists
                   (fn (ship' : Ship.ship, _, _) => ship' = ship)
                   placngs orelse
                   List.exists
                   (fn (ship' : Ship.ship, _, _) => ship' = ship)
                   nonUniqs
                then split(uniqs, placng :: nonUniqs, placngs)
                else split(placng :: uniqs, nonUniqs, placngs)
      in split(nil, nil, placngs) end

fun iterate(rows, placngs) =
      let val placngs           = List.filter (consistent rows) placngs
          val (uniqs, nonUniqs) = splitPlacingsByShipUniqueness placngs
      in if null uniqs
         then (rows, nonUniqs)
         else let fun apply(nil,           rows) = rows
                    | apply(uniq :: uniqs, rows) =
                        apply(uniqs, applyPlacing(rows, uniq))
               in iterate(apply(uniqs, rows), nonUniqs) end
      end

fun miss((rows, placngs), pos) =
      (update(rows, Board.posToPair pos, Miss), placngs)

fun hit((rows, placngs), pos) =
      (update(rows, Board.posToPair pos, Hit NONE), placngs)

fun sank((rows, placngs), pos, ship) =
      iterate
      (update(rows, Board.posToPair pos, Hit(SOME ship)),
       placngs @ placings(ship, pos))

fun cellToChar UnShot           = #" "
  | cellToChar Miss             = #"*"
  | cellToChar (Hit NONE)       = #"+"
  | cellToChar (Hit(SOME ship)) = Char.toUpper(Ship.shipToLetter ship)

fun boardToStrs(rows, placngs) =
      MatrixUntrusted.matrixToStrs(cellToChar, Board.size, Board.size) rows @
      placingsToStrs placngs

end;
