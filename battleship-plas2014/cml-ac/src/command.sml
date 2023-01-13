(* command-oriented processing via internet connection *)

structure Command :> COMMAND =
struct

type socket = INetSock.inet SockUtil.stream_sock

fun sendLineToSock(sock, x) =
      SockUtil.sendStr(sock, x ^ "\n")
        handle _ => ()  (* if socket closed at other side, ignore exception *)

(* trailing newline is stripped off; returns NONE if the other side of
   the connection is closed *)

fun recvLineFromSock sock =
      let fun loop xs =
                case SOME(SockUtil.recvStr(sock, 1))
                       handle _ => NONE of
                     NONE   => NONE (* other side closed connection *)
                   | SOME x =>
                       if x = "\n"
                       then SOME(String.concat(rev xs))
                       else loop(x :: xs)
      in loop nil end

(* messages sent from server to client *)

datatype server_msg =
         DoneServerMsg
       | InfoServerMsg of string
       | PromptServerMsg of string

(* used by server *)

fun sendServerMsgToSock(sock, msg) =
      case msg of
           DoneServerMsg     => sendLineToSock(sock, ".")
         | InfoServerMsg x   => sendLineToSock(sock, ">" ^ x)
         | PromptServerMsg x => sendLineToSock(sock, "<" ^ x)

(* used by the client loop *)

fun recvServerMsgFromSock sock =
      case recvLineFromSock sock of
           NONE     => DoneServerMsg  (* server closed connection *)
         | SOME "." => DoneServerMsg
         | SOME x   =>
             (case String.sub(x, 0) of
                   #">" => InfoServerMsg(String.extract(x, 1, NONE))
                 | #"<" => PromptServerMsg(String.extract(x, 1, NONE))
                 | _    => DoneServerMsg  (* shouldn't happen *))

(* server functions *)

fun doneServer sock = sendServerMsgToSock(sock, DoneServerMsg)

fun infoServer(sock, msgs) =
      app
      (fn msg => sendServerMsgToSock(sock, InfoServerMsg msg))
      msgs

fun promptServer(sock, prompt, quitMsg) =
      (sendServerMsgToSock(sock, PromptServerMsg prompt);
       case recvLineFromSock sock of
            NONE   => NONE
          | SOME x => if x = quitMsg then NONE else SOME x)

(* client loop *)

fun clientLoop(stdIn, stdOut, sock, quitMsg) =
      case recvServerMsgFromSock sock of
           DoneServerMsg     => ()
         | InfoServerMsg x   =>
             (TextIO.output(stdOut, x ^ "\n");
              clientLoop(stdIn, stdOut, sock, quitMsg))
         | PromptServerMsg x =>
             (TextIO.output(stdOut, x ^ "\n");
              case TextIO.inputLine stdIn of
                   NONE   =>  (* end of file *)
                     (sendLineToSock(sock, quitMsg);
                      clientLoop(stdIn, stdOut, sock, quitMsg))
                 | SOME y =>
                     (sendLineToSock(sock, Aux.strip y);
                      clientLoop(stdIn, stdOut, sock, quitMsg)))

end;
