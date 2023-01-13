(* main processing *)

(* Trusted module implementing program's entry point. *)

signature MAIN =
sig

(* entry point to program, which takes the name by which the program
   was invoked, plus its command line arguments

   when main is run, it matches the program's command line arguments
   against the following patterns, issuing an error message if pattern
   matching fails:

   ["server", port]

     starts the server running on the specified port

   ["client", host, port]

     starts an instance of the client, specifying the host and port to
     connect to *)

val main : string * string list -> OS.Process.status

end;
