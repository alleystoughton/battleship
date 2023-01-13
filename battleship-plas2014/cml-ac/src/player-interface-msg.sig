(* communication of messages between player interfaces *)

(* Trusted module implementing the datatype of messages exchanged by
   player interfaces, as well as a function for creating a player
   interface message pair (send, recv), where send is used to send a
   message to the other player interface, and recv is used to receive
   a message from the other player interface. *)

signature PLAYER_INTERFACE_MSG =
sig

(* messages exchanged by player interfaces *)

datatype pi_msg =
           TLB of Board.tlb  (* a player's totally locked board *)
         | Resign  (* resignation *)
         | Pos of Board.pos  (* position to shoot *)
         | CK of Board.ck  (* shooting response -- counted key *)

(* player interface message pair *)

type pi_msg_pair = (pi_msg -> unit) * (unit -> pi_msg)

(* create a player interface message pair (send, recv), referencing a
   new, private channel, ch

   if one thread executes send msg, while another executes recv(),
   then both calls can succeed, returning () and msg, respectively

   the above synchronization will eventually succeed, assuming no
   other threads call send or recv before it succeeds *)

val piMsgPair : unit -> pi_msg_pair

end;
