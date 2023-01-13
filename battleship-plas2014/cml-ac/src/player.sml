(* server side of player *)

structure Player :> PLAYER =
struct

open CML

(* fatal error message on behalf of player *)

fun fatal msg = Aux.fatal("player", msg)

(* regular expression describing a legal board coordinate *)

val coorReg =
      implode[#"[", #"a", #"-", chr(ord #"a" + Board.size - 1), #"]"]

fun getShotPos sock =
       case Command.promptServer
            (sock,
             "take your next shot: (" ^ coorReg ^ coorReg ^ ")",
             "resign") of
            NONE          => NONE
          | SOME "resign" => NONE
          | SOME x        =>
              (case Board.strToPos x of
                    NONE =>
                      (Command.infoServer(sock, ["syntax error"]);
                       getShotPos sock)
                  | res  => res)

fun parsePosOrient x =
      case String.tokens (fn c => c = #":") x of
           [y, z] =>
             (case Board.strToPos y of
                   NONE     => NONE
                 | SOME pos =>
                     case Board.strToOrient z of
                          NONE        => NONE
                        | SOME orient => SOME(pos, orient))
         | _      => NONE

fun getShipPosOrient(sock, ship) =
       case Command.promptServer
            (sock,
             "place your " ^ Ship.shipToStr ship ^
             " (size " ^ Int.toString(Ship.size ship) ^ ", " ^
             coorReg ^ coorReg ^ ":[h+v])",
             "resign") of
            NONE          => NONE
          | SOME "resign" => NONE
          | SOME x        =>
              (case parsePosOrient x of
                    NONE =>
                      (Command.infoServer(sock, ["syntax error"]);
                       getShipPosOrient(sock, ship))
                  | res  => res)

datatype shot_result =
           Repeat             (* an attempt to shoot a position already shot *)
         | Miss               (* miss -- no ship hit *)
         | Hit                (* hit unspecified ship *)
         | Sank of Ship.ship  (* sank a ship -- last of its cells hit *)

(* shooting phase commands *)

datatype cmd =
           Shoot        of (Board.pos * (shot_result -> unit))option chan
         | OpponentShot of Board.pos
         | Won
         | Lost

type shooting_handle = cmd chan

type placing_handle = (Board.complete * shooting_handle)option chan

fun shootingLoop(sock, shootingCh, board, shotBoard) =
      (Command.infoServer(sock, ["[Your Board]"]);
       Command.infoServer(sock, BoardUntrusted.boardToStrs board);
       Command.infoServer(sock, ["[Opponent's Board]"]);
       Command.infoServer(sock, ShotBoard.boardToStrs shotBoard);
       case recv shootingCh of
            Shoot replyCh    =>
              (case getShotPos sock of
                    NONE     =>
                      (send(replyCh, NONE);
                       shootingLoop(sock, shootingCh, board, shotBoard))
                  | SOME pos =>
                      let val shotCh = channel()
                      in send
                         (replyCh,
                          SOME(pos, fn shot => send(shotCh, shot)));
                         case recv shotCh of
                              Repeat    =>
                                (Command.infoServer(sock, ["shot repetition"]);
                                 shootingLoop
                                 (sock, shootingCh, board, shotBoard))
                            | Miss      =>
                                (Command.infoServer
                                 (sock, ["you missed a ship"]);
                                 let val shotBoard =
                                           ShotBoard.miss(shotBoard, pos)
                                 in shootingLoop
                                    (sock, shootingCh, board, shotBoard)
                                 end)
                            | Hit       =>
                                (Command.infoServer
                                 (sock, ["you hit a ship"]);
                                 let val shotBoard =
                                           ShotBoard.hit(shotBoard, pos)
                                 in shootingLoop
                                    (sock, shootingCh, board, shotBoard)
                                 end)
                            | Sank ship =>
                                (Command.infoServer
                                 (sock,
                                  ["you sank the " ^ Ship.shipToStr ship]);
                                 let val shotBoard =
                                           ShotBoard.sank(shotBoard, pos, ship)
                                 in shootingLoop
                                    (sock, shootingCh, board, shotBoard)
                                 end)
                      end)
          | OpponentShot pos =>
              let val (board, sh) = BoardUntrusted.shoot(board, pos)
              in case sh of
                      BoardUntrusted.Repeat    => fatal "player interface error"
                    | BoardUntrusted.Miss      =>
                        Command.infoServer
                        (sock,
                         ["opponent shot cell " ^ Board.posToStr pos ^
                          " but missed"])
                    | BoardUntrusted.Hit ship  =>
                        Command.infoServer
                        (sock,
                         ["opponent shot cell " ^ Board.posToStr pos ^
                          " and hit your " ^ Ship.shipToStr ship])
                    | BoardUntrusted.Sank ship =>
                        Command.infoServer
                        (sock,
                         ["opponent shot cell " ^ Board.posToStr pos ^
                          " and sank your " ^ Ship.shipToStr ship]);
                 shootingLoop(sock, shootingCh, board, shotBoard)
              end
          | Won              =>
              (Command.infoServer(sock, ["you won!"]);
               Command.doneServer sock)
          | Lost             =>
              (Command.infoServer(sock, ["you lost!"]);
               Command.doneServer sock))

fun placingLoop(sock, placingCh, placing, nil)           =
      let val _          =
                Command.infoServer
                (sock,
                 BoardUntrusted.placingToStrs placing @
                 ["", "SHOOTING PHASE", ""])
          val shootingCh = channel()
          val complete   = valOf(Board.placingToComplete placing)
      in send(placingCh, SOME(complete, shootingCh));
         shootingLoop
         (sock, shootingCh, BoardUntrusted.completeToBoard complete,
          ShotBoard.empty)
      end
  | placingLoop(sock, placingCh, placing, ship :: ships) =
      (Command.infoServer(sock, BoardUntrusted.placingToStrs placing);
       case getShipPosOrient(sock, ship) of
            NONE              =>
              (Command.doneServer sock; send(placingCh, NONE))
          | SOME(pos, orient) =>
              (case Board.place(placing, ship, pos, orient) of
                    Board.PlacingSuccess placing =>
                      placingLoop(sock, placingCh, placing, ships)
                  | Board.DuplicateShip          => raise Fail "cannot happen"
                  | Board.OffBoard               =>
                      (Command.infoServer
                       (sock, ["ship (partially) off board"]);
                       placingLoop(sock, placingCh, placing, ship :: ships))
                  | Board.OverlapsShip           =>
                      (Command.infoServer
                       (sock, ["ship overlaps other ship"]);
                       placingLoop(sock, placingCh, placing, ship :: ships))))

val shipsInfo =
      List.map
      (fn ship =>
            str(Ship.shipToLetter ship) ^ " = " ^
            Ship.shipToStr ship ^
            " (size " ^ Int.toString(Ship.size ship) ^ ")")
      Ship.ships

fun placing(sock, goFirst) =
      let val placingCh = channel()
      in Command.infoServer
         (sock,
          ["Welcome to Battleship!",
           "",
           "At any prompt, you may type \"resign\" or signal end-of-file " ^
           "to resign.",
           "Ships:"] @
          map (fn s => "  " ^ s ^ ",") (Aux.allButLast shipsInfo) @
          ["  " ^ List.last shipsInfo ^ "."] @
          ["Cell positions consist of row then column; e.g., df is row d, " ^
           "column f.",
           "Ship placements consist of row/column of leftmost/topmost cell,",
           "  followed by h for horizontal or v for vertical.",
           "  E.g., df:h means horizontal placement with leftmost cell at df.",
           "  E.g., df:v means vertical placement with topmost cell at df.",
           "Shot cells:",
           "  * = miss,",
           "  uppercase/+ = hit.",
           "",
           "You are Player " ^ (if goFirst then "1" else "2"),
           "",
           "PLACING PHASE",
           ""]);
         spawn(fn () => placingLoop(sock, placingCh, Board.empty, Ship.ships));
         placingCh
      end

val getComplete = recv
      
fun shoot shootingCh =
      let val replyCh = channel()
      in send(shootingCh, Shoot replyCh); recv replyCh end
      
fun opponentShot(shootingCh, pos) = send(shootingCh, OpponentShot pos)

fun won shootingCh = send(shootingCh, Won)

fun lost shootingCh = send(shootingCh, Lost)

end;
