(* unforgeable keys *)

structure Key :> KEY =
struct

type key = IntInf.int  (* keys are implemented as integers *)

(* channel from which newKey obtains each new key, via a synchronous
   communication *)

val keyCh : key CML.chan = CML.channel()

(* the key server sends new keys on the key channel: 0, 1, ... *)

(* initialize (start) the key server *)

local
  fun keyServer(ctr : IntInf.int) = (CML.send(keyCh, ctr); keyServer(ctr + 1))
in
  fun keyServerInit() = (CML.spawn(fn () => keyServer 0); ())
end

(* finalize the key server -- does nothing *)

fun keyServerFinal() = ()

(* register the key channel and server

   registering the server causes the server to be initialized
   (started), when CML starts up; when CML is shut down, it causes the
   server to be finalized *)

val _ =
      (RunCML.logChannel("key channel", keyCh);
       RunCML.logServer("key server", keyServerInit, keyServerFinal))

fun newKey() : key = CML.recv keyCh

fun sameKey(key1 : key, key2) = key1 = key2

end;
