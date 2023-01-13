(* client slide of player *)

structure Client :> CLIENT =
struct

fun client(stdIn, stdOut, sock) =
      (TextIO.output(stdOut, "connected\n");
       Command.clientLoop(stdIn, stdOut, sock, "resign"))

end;
