(* small matrices -- used to represent game boards *)

structure Matrix :> MATRIX =
struct

exception Invalid

type 'a matrix = 'a list list

fun valid(ht, wid, rows) =
      ht >= 1 andalso wid >= 1                   andalso
      ht <= 26 andalso wid <= 26                 andalso
      length rows = ht                           andalso
      List.all (fn row => length row = wid) rows

fun const(ht, wid, x) =
      if ht >= 1 andalso wid >= 1   andalso
         ht <= 26 andalso wid <= 26
      then let val row = List.tabulate(wid, fn _ => x)
           in List.tabulate(ht, fn _ => row) end
      else raise Invalid

fun sub(ht, wid, rows, i, j) =
      if valid(ht, wid, rows)   andalso
         i >= 0 andalso i < ht  andalso
         j >= 0 andalso j < wid
      then List.nth(List.nth(rows, i), j)
      else raise Invalid

fun update(ht, wid, rows, i, j, x) =
      if valid(ht, wid, rows)   andalso
         i >= 0 andalso i < ht  andalso
         j >= 0 andalso j < wid
      then let val row = List.nth(rows, i)
               val row = Aux.update(row, j, x)
           in Aux.update(rows, i, row) end
      else raise Invalid

fun map f = List.map(List.map f)

fun transpose(ht, wid, rows) =
      let fun trans rows  =
                if length(hd rows) = 1
                then [List.map hd rows]
                else List.map hd rows :: trans(List.map tl rows)
      in if valid(ht, wid, rows)
         then trans rows
         else raise Invalid
      end

end;
