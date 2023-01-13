(* player interfaces *)

functor PlayerInterface(structure Player : PLAYER) :> PLAYER_INTERFACE =
struct

structure PIM = PlayerInterfaceMsg

fun fatal msg = Aux.fatal("player interface", msg)

fun catch f = Aux.catch ("player interface", "player raised exception") f

fun ownTurn(sh, (send, recv), ownBoard, oppBoard, keyCtr) =
      if Board.keyedAllSunk ownBoard  (* last shot (if any) was by opponent *)
      then catch Player.lost sh
      else case catch Player.shoot sh of
                NONE             => (send PIM.Resign; catch Player.lost sh)
              | SOME(pos, reply) =>
                  if Board.lockedAlreadyShot(oppBoard, pos)
                  then (catch reply Player.Repeat;
                        ownTurn(sh, (send, recv), ownBoard, oppBoard, keyCtr))
                  else (send(PIM.Pos pos);
                        case recv() of
                             PIM.CK ck =>
                               let val (oppBoard, lsr) =
                                         Board.lockedShoot(oppBoard, pos, ck)
                               in case lsr of
                                       Board.InvalidLSR   =>
                                         fatal "protocol error"
                                     | Board.RepeatLSR    =>
                                         raise Fail "cannot happen"
                                     | Board.MissLSR      =>
                                         (catch reply Player.Miss;
                                          oppTurn
                                          (sh, (send, recv), ownBoard,
                                           oppBoard, keyCtr))
                                     | Board.HitLSR       =>
                                         (catch reply Player.Hit;
                                          oppTurn
                                          (sh, (send, recv), ownBoard,
                                           oppBoard, keyCtr))
                                     | Board.SankLSR ship =>
                                         (catch reply (Player.Sank ship);
                                          oppTurn
                                          (sh, (send, recv), ownBoard,
                                           oppBoard, keyCtr))
                               end
                           | _         => fatal "protocol error")

and oppTurn(sh, (send, recv), ownBoard, oppBoard, keyCtr) =
      if Board.lockedAllSunk oppBoard  (* last shot (if any) was by me *)
      then catch Player.won sh
      else case recv() of
                PIM.Resign  => catch Player.won sh
              | PIM.Pos pos =>
                  let val (ownBoard, keyOpt) = Board.keyedShoot(ownBoard, pos)
                  in case keyOpt of
                          NONE     => fatal "protocol error"
                        | SOME key =>
                            (send(PIM.CK(Board.labelKey(key, keyCtr)));
                             catch Player.opponentShot (sh, pos);
                             ownTurn
                             (sh, (send, recv), ownBoard, oppBoard,
                              keyCtr + 1))
                  end
              | _           => fatal "protocol error"

fun start(ph, (send, recv), goFirst) =
      case catch Player.getComplete ph of
           NONE            =>
             if goFirst
             then send PIM.Resign
             else (case recv() of
                        PIM.Resign => ()
                      | _          => send PIM.Resign)
         | SOME(compl, sh) =>
             let val (keyedBoard, tlbFun) =
                       Board.completeToBoardPair compl
                 val tlb                  = tlbFun goFirst
             in if goFirst
                then (send(PIM.TLB tlb);
                      case recv() of
                           PIM.Resign     => catch Player.won sh
                         | PIM.TLB oppTLB =>
                             if Board.idOfTLB oppTLB = goFirst
                             then fatal "protocol error"
                             else ownTurn
                                  (sh, (send, recv), keyedBoard,
                                   Board.lbOfTLB oppTLB, 0)
                         | _              => fatal "protocol error")
                else case recv() of
                           PIM.Resign     => catch Player.won sh
                         | PIM.TLB oppTLB =>
                             if Board.idOfTLB oppTLB = goFirst
                             then fatal "protocol error"
                             else (send(PIM.TLB tlb);
                                   oppTurn
                                   (sh, (send, recv), keyedBoard,
                                    Board.lbOfTLB oppTLB, 0))
                         | _              => fatal "protocol error"
             end

fun piSpawn(sock, msgPair, goFirst) =
      let val ph = catch Player.placing (sock, goFirst)
      in CML.spawn(fn () => start(ph, msgPair, goFirst)) end

end;
