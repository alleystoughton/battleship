(* the referee *)

functor Referee
        (structure Player1 : PLAYER
         structure Player2 : PLAYER) :>
        REFEREE =
struct

fun catch1 f = 
      Aux.catch ("referee", "player 1 function raised exception") f

fun catch2 f = 
      Aux.catch ("referee", "player 2 function raised exception") f

fun first(sh1, board1, sh2, board2) =
      case catch1 Player1.shoot sh1 of
           NONE             => (catch1 Player1.lost sh1; catch2 Player2.won sh2)
         | SOME(pos, reply) =>
             let val (board2, shot) = Board.shoot(board2, pos)
             in case shot of
                     Board.Repeat    =>
                       (catch1 reply Player1.Repeat;
                        first(sh1, board1, sh2, board2))
                   | Board.Miss      =>
                       (catch1 reply Player1.Miss;
                        catch2 Player2.opponentShot (sh2, pos);
                        second(sh1, board1, sh2, board2))
                   | Board.Hit _     =>
                       (catch1 reply Player1.Hit;
                        catch2 Player2.opponentShot (sh2, pos);
                        second(sh1, board1, sh2, board2))
                   | Board.Sank ship =>
                       (catch1 reply (Player1.Sank ship);
                        catch2 Player2.opponentShot (sh2, pos);
                        if Board.allSunk board2
                        then (catch1 Player1.won sh1; catch2 Player2.lost sh2)
                        else second(sh1, board1, sh2, board2))
             end

and second(sh1, board1, sh2, board2) =
      case catch2 Player2.shoot sh2 of
           NONE             => (catch2 Player2.lost sh2; catch1 Player1.won sh1)
         | SOME(pos, reply) =>
             let val (board1, shot) = Board.shoot(board1, pos)
             in case shot of
                     Board.Repeat    =>
                       (catch2 reply Player2.Repeat;
                        second(sh1, board1, sh2, board2))
                   | Board.Miss      =>
                       (catch2 reply Player2.Miss;
                        catch1 Player1.opponentShot (sh1, pos);
                        first(sh1, board1, sh2, board2))
                   | Board.Hit _     =>
                       (catch2 reply Player2.Hit;
                        catch1 Player1.opponentShot (sh1, pos);
                        first(sh1, board1, sh2, board2))
                   | Board.Sank ship =>
                       (catch2 reply (Player2.Sank ship);
                        catch1 Player1.opponentShot (sh1, pos);
                        if Board.allSunk board1
                        then (catch2 Player2.won sh2; catch1 Player1.lost sh1)
                        else first(sh1, board1, sh2, board2))
             end

fun referee(sock1, sock2) =
      let val ph1 = catch1 Player1.placing (sock1, true)
          val ph2 = catch2 Player2.placing (sock2, false)
      in case catch1 Player1.getComplete ph1 of
              NONE              =>
                (case catch2 Player2.getComplete ph2 of
                      NONE         => ()
                    | SOME(_, sh2) => catch2 Player2.won sh2)
            | SOME(compl1, sh1) =>
                case catch2 Player2.getComplete ph2 of
                     NONE              => catch1 Player1.won sh1
                   | SOME(compl2, sh2) =>
                       first
                       (sh1, Board.completeToBoard compl1,
                        sh2, Board.completeToBoard compl2)
      end

end;
