(* command-oriented processing via internet connection *)

(* Untrusted module implementing protocol by which the client and
   server sides of a player communicate, over a TCP connection. *)

signature COMMAND =
sig

(* internet socket, providing a two-way connection between a client
   and a server *)

type socket = INetSock.inet SockUtil.stream_sock

(* server functions *)

(* tell the client to exit *)

val doneServer : socket -> unit

(* tell the client to display a list of strings, as separate lines,
   in order *)

val infoServer : socket * string list -> unit

(* promptServer(sock, prompt, quitMsg) tells the client to prompt its
   user for a line of input, using prompt, and then send this input --
   stripped of leading/trailing whitespace -- to the server, so it may
   be returned, decorated by SOME, as the result of promptSever

   quitMsg (which should not have leading or trailing whitespace)
   should be the same *quit message* passed to clientLoop; if the
   client's user signals end-of-file, or enters quitMsg (optionally
   surrounded by whitespace) as its line, then the client will send
   quitMsg to the server, causing promptServer to return NONE

   if the client's user interrupts (usually CTRL-c) (or has already
   interrupted at the point when promptServer is called), then
   promptServer will also return NONE *)

val promptServer : socket * string * string -> string option

(* client function *)

(* clientLoop(stdIn, stdOut, sock, quitMsg) runs the client loop,
   communicating with the user via stdIn/stdOut, and with the server
   via sock; it uses quitMsg (see promptServer) as the quit message *)

val clientLoop : TextIO.instream * TextIO.outstream * socket * string -> unit

end;
