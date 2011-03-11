open Bricabrac
open Editor

let load_all_plugins dir =
	Repl.eval repl ("#directory \""^dir^"\"") ;
	Repl_buf.append repl (Repl_buf.Rope.of_string ("Loading plugins from "^dir^"...\n")) ;
	let load_plugin name stat =
		if stat.Unix.st_kind = Unix.S_REG then (
			Log.p "Loading plugin %s" name ;
			Repl.eval repl ("#load \""^name^"\"")
		) in
	foreach_file dir load_plugin

let _ =
	let instdir = Sys.getcwd () in
	load_all_plugins (instdir^"/plugins")
