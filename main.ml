open Editor

(* As this module will not be available from the toplevel (value not already computed),
 * we'd rather have nothing interresting in there. *)

let start =
	let instdir = Sys.getcwd () in
	Repl.eval repl ("#load \""^instdir^"/system.cmo\"") ;
	Repl.eval repl ("#use \""^(Unix.getenv "HOME")^"/.otrui.rc\"") ;
	Repl.eval repl "#use \"./.otrui.rc\"" ;
	Editor.start ()

