(* server side of player *)

(* Untrusted module implementing the server side of a player, which
   communicates with its client side via TCP. A manual check must be
   made that a client only communicates via its interface.  When the
   server prompts the client for input, it uses "resign" as the quit
   message (see COMMAND). *)

signature PLAYER =
sig

(* *placing handle* for player -- handle for player during the placing
   phase of game *)

type placing_handle

(* *shooting handle* for player -- handle for player during the
   shooting phase of game *)

type shooting_handle

(* placing(sock, goFirst) starts up a new player, returning its
   placing handle; the player will use sock to communicate with its
   client side, using the protocol of COMMAND, and will go (shoot)
   first iff goFirst is true *)

val placing : Command.socket * bool -> placing_handle

(* getComplete ph waits for the player corresponding to the
   placing handle ph to finish initialization

   if the player resigns or terminates before fully initializing, then
   getComplete returns NONE; otherwise, getComplete returns
   SOME(compl, sh), where compl is the complete placing chosen
   by the player, and sh is the shooting handle for the player

   getComplete may not be called more than once on a given
   placing handle *)

val getComplete : placing_handle -> (Board.complete * shooting_handle)option

(* shot results communicated to shooting player *)

datatype shot_result =
           Repeat             (* an attempt to shoot a position already shot *)
         | Miss               (* miss -- no ship hit *)
         | Hit                (* hit an unspecified ship *)
         | Sank of Ship.ship  (* sank a ship -- last of its cells hit *)

(* shoot sh waits for the player with shooting handle sh to supply a
   value of type (Board.pos * (shot_result -> unit))option

   if NONE is supplied, this means the player is resigning

   if SOME(pos, shotFun) is returned, this means the player has chosen
   to shoot cell pos of the opponent's board; the caller of shoot is
   then responsible for calling shotFun with the value of type
   shot_result describing the outcome of this shot (this must be done
   before any of the module's functions are called with sh) *)

val shoot : shooting_handle -> (Board.pos * (shot_result -> unit))option

(* opponentShot(sh, pos) tells the player with shooting handle sh
   that its opponent has shot cell pos of the player's board *)

val opponentShot : shooting_handle * Board.pos -> unit

(* won sh tells the player with shooting handle sh that it has
   won the game

   once won has been called with a given shooting handle, no subsequent
   use of that shooting handle may be made *)

val won : shooting_handle -> unit

(* lost sh tells the player with shooting handle sh that it has
   lost the game

   once lost has been called with a given shooting handle, no subsequent
   use of that shooting handle may be made *)

val lost : shooting_handle -> unit

end;
