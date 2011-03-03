open Bricabrac
module Rope = Buf.Rope

let start =
	let instdir = Sys.getcwd () in
	Buf.repl#eval ("#load \""^instdir^"/system.cmo\"") ;
	Buf.repl#eval ("#use \""^(Unix.getenv "HOME")^".otrui.rc\"") ;
	Buf.repl#eval "#use \"./.otrui.rc\"" ;
	Cmd.key_loop ()

