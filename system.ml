open Buf
open Bricabrac

let load_all_plugins () =
	repl#append (Rope.of_string "Loading plugins...\n") ;
	let load_plugin name stat =
		if stat.Unix.st_kind = Unix.S_REG then (
			Log.p "Loading plugin %s" name ;
			Buf.repl#eval ("#load \""^name^"\"")
		) in
	foreach_file "./plugins" load_plugin

let _ = load_all_plugins ()
