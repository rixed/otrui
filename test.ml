open Bricabrac

(* works only if a function... If not, cannot access to these symbols :-/ *)
let start () =
	Term.init () ;
	Cmd.key_loop ()

