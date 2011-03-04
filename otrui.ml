open Bricabrac
module Rope = Buf.Rope

let start =
	let instdir = Sys.getcwd () in
	Repl.repl#eval ("#load \""^instdir^"/system.cmo\"") ;
	Repl.repl#eval ("#use \""^(Unix.getenv "HOME")^".otrui.rc\"") ;
	Repl.repl#eval "#use \"./.otrui.rc\"" ;
	Cmd.key_loop ""

