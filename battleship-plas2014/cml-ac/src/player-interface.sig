(* player interfaces *)

(* Trusted module implementing player interfaces, which are
   secure against each other.  Each player interface creates its own
   player (see PLAYER).  Player interfaces communicate using the
   mechanism of PLAYER_INTERFACE_MSG.

   If one of a pair of player interfaces is untrusted, a manual check
   must be made that it only communicates via its interface. *)

signature PLAYER_INTERFACE =
sig

(* piSpawn(sock, (send, recv), goFirst) spawns a player interface
   thread, yielding a thread ID

   sock is used to communicate with the player's client side

   send is used to send a player interface message to the other
   player interface

   recv is used to receive a player interface message from the
   other player interface

   goFirst is true iff the player interface is supposed to take
   the first (3rd, 5th, etc.) turn *)

val piSpawn :
      Command.socket * PlayerInterfaceMsg.pi_msg_pair * bool -> CML.thread_id

end;

(* A structure with signature PLAYER_INTERFACE is formed using the functor
   PlayerInterface (see player-interface.sml):

     functor PlayerInterface(structure Player : PLAYER) :> PLAYER_INTERFACE =
     ...

   The piSpawn function uses the structure Player *)
