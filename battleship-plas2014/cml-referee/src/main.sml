(* main processing *)

structure Main :> MAIN =
struct

open CML

structure Referee :> REFEREE =
  Referee
  (structure Player1 = Player
   structure Player2 = Player)

(* ignore SIGPIPE signal -- generated when trying to write to a socket
   that's been closed at the other end, e.g., because the process at
   the other end has been interrupted *)

fun ignoreSigPIPE() =
      UnixSignals.setHandler (UnixSignals.sigPIPE, UnixSignals.IGNORE)

fun error msg = (print(msg ^ "\n"); OS.Process.failure)

(* server *)

fun server port =
      let val sock = INetSock.TCP.socket()
          val bind =
                (Socket.bind(sock, INetSock.any port); true)
                  handle _ => false
      in if bind
         then (ignoreSigPIPE();
               Socket.listen(sock, 2);
               print("listening on port " ^ Int.toString port ^ " ...\n");
               let val (sock1, _) = Socket.accept sock
                   val _          = print "Player 1 connected\n"
                   val (sock2, _) = Socket.accept sock
                   val _          = print "Player 2 connected\n"
                   val _          = Socket.close sock
               in Referee.referee(sock1, sock2);
                  print "shutting down\n"; OS.Process.success
               end)
         else error("port invalid or already in use")
      end

(* client side of player *)

fun client(addr, port) =
      let val sock = INetSock.TCP.socket()
          val bind =
                (Socket.connect(sock, INetSock.toAddr(addr, port)); true)
                  handle _ => false
      in if bind
         then (Aux.catch
               ("main", "client raised exception")
               Client.client
               (TextIO.stdIn, TextIO.stdOut, sock);
               OS.Process.success)
         else error "cannot connect to port on host"
      end

(* entry point *)

fun usage prog =
      (print
       ("usage:\n" ^
        "  " ^ prog ^ " server PORT\n" ^
        "  " ^ prog ^ " client HOST PORT\n");
       OS.Process.failure)

fun main(prog, ["server", port])       =
      (case Aux.strToInt port of
            NONE      => error "invalid port"
          | SOME port =>
              if port >= 0
              then RunCML.doit
                   (fn () => RunCML.shutdown(server port),
                    NONE)
              else error "invalid port")
  | main(prog, ["client", host, port]) =
      (case Aux.strToInt port of
            NONE      => error "invalid port"
          | SOME port =>
              if port >= 0
              then case NetHostDB.getByName host of
                        NONE       => error "invalid host"
                      | SOME entry =>
                          let val addr = NetHostDB.addr entry
                          in RunCML.doit
                             (fn () => RunCML.shutdown(client(addr, port)),
                              NONE)
                          end
              else error "invalid port")
  | main(prog, _)                      = usage prog

end;
