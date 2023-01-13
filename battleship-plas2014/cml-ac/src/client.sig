(* client side of player *)

(* Untrusted module implementing the client side of a player, which
   communicates with its server side via TCP. A manual check must be
   made that a client only communicates via its interface. *)

signature CLIENT =
sig

(* client(stdIn, stdOut, sock) starts the client side of a player,
   using stdIn/stdOut for communicating with the user's terminal,
   and sock for communicating with the player's server side *)

val client : TextIO.instream * TextIO.outstream * Command.socket -> unit

end;
