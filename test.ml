open Bricabrac

(* works only if a function... If not, cannot access to these symbols :-/ *)
let start () =
	Term.init () ;
	View.init () ;
	Win.init () ;
	Cmd.init () ;
	Cmd.key_loop ()

