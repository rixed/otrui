open Buf
open Bricabrac

let load_all_plugins dir =
	Repl.repl#eval ("#directory \""^dir^"\"") ;
	Repl.repl#append (Rope.of_string ("Loading plugins from "^dir^"...\n")) ;
	let load_plugin name stat =
		if stat.Unix.st_kind = Unix.S_REG then (
			Log.p "Loading plugin %s" name ;
			Repl.repl#eval ("#load \""^name^"\"")
		) in
	foreach_file dir load_plugin

let _ =
	let instdir = Sys.getcwd () in
	load_all_plugins (instdir^"/plugins")
