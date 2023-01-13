(* the referee *)

(* Trusted module implementing the referee that mediates between
   untrusted players. *)

signature REFEREE =
sig

(* referee(sock1, sock2) runs the referee, using the supplied sockets
   for communicating with the client sides of the two players

   referee returns when the game is over *)

val referee : Command.socket * Command.socket -> unit

end;

(* A structure with signature REFEREE is formed using the functor
   Referee (see referee.sml):

     functor Referee
             (structure Player1 : PLAYER
              structure Player2 : PLAYER) :>
             REFEREE = ...

   The referee function uses the structures Player1 and Player2. *)
