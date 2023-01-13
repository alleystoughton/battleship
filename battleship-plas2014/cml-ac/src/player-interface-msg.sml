(* communication of messages between player interfaces *)

structure PlayerInterfaceMsg :> PLAYER_INTERFACE_MSG =
struct

datatype pi_msg =
           TLB of Board.tlb
         | Resign
         | Pos of Board.pos
         | CK of Board.ck

type pi_msg_pair = (pi_msg -> unit) * (unit -> pi_msg)

fun piMsgPair() =
      let val ch = CML.channel()
      in (fn msg => CML.send(ch, msg), fn () => CML.recv ch) end

end;
