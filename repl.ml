(* 1st method: use toplevellib (compile with toplevellib.cma)
Toploop.initialize_toplevel_env();;

let eval txt =
	let lb = (Lexing.from_string txt) in
	let phr = !Toploop.parse_toplevel_phrase lb in
    Toploop.execute_phrase true Format.std_formatter phr;;

eval "let add1 x = x +1;;";;
eval "add1 2;;";;
*)

(* 2nd method: run the app from the toplevel, but redirect stdin/out/err
 * into pipes that are read/written by the program loaded from the toplevel.
 * In other words, a module started by the toplevel seizes control of the toplevel.
 *)

let redirect_out_to_file fd =
	Unix.dup2 fd Unix.stdout ;
	Unix.dup2 fd Unix.stderr ;
	Unix.close fd

let redirect_in_from_file fd =
	Unix.dup2 fd Unix.stdin ;
	Unix.close fd

(* Returns two channels, one where to write commands, the other to read results *)
let seize_toplevel () =
	let out_read, out_write = Unix.pipe () in
	redirect_out_to_file out_write ;
	let in_read, in_write = Unix.pipe () in
	redirect_in_from_file in_read ;
	Unix.out_channel_of_descr in_write, Unix.in_channel_of_descr out_read

let play_with_repl (repl_in,repl_out) =
	let ochn = open_out "/tmp/repl.log" in
	Printf.fprintf ochn "Seized toplevel!\n%!" ;
	Printf.fprintf ochn "Got: %s\n%!" (input_line repl_out) ;
	Printf.fprintf repl_in "String.length \"blablabla\";;\n%!" ;
	Printf.fprintf ochn "Sent command!\n%!" ;
	let res = input_line repl_out in
	Printf.fprintf ochn "Got: %s\n%!" res ;
	close_out ochn

let _ =
	let repl_in, repl_out = seize_toplevel () in
	(* Do not write into repl_in until the toplevel is actually reading, or we will deadblock.
	 * The solution here is to start a new thread *)
	ignore (Thread.create play_with_repl (repl_in, repl_out))

