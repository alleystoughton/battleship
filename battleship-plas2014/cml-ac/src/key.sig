(* unforgeable keys *)

(* Trusted module implementing abstract type of unforgeable keys. This
   module is thread-safe. *)

signature KEY =
sig

(* unforgeable keys *)

type key

(* generate a new key, guaranteed to be distinct from all previously
   generated keys *)

val newKey : unit -> key

(* test keys for equality *)

val sameKey : key * key -> bool

end;
