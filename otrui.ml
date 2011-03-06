open Bricabrac
module Rope = Buf.Rope

type mode = Command | Insert
let mode = ref Insert
let command = ref []
let auto_insert = ref true	(* return in insert mode once command is executed *)

let add_key k =
	Log.p "Got key %d" k ;
	let focused = try Some (Win.view_of !Win.root) with Not_found -> None in
	if focused = None then mode := Command ;

	if k = Term.ascii_escape then (
		if focused <> None then (
			mode := (match !mode with Command -> Insert | Insert -> Command) ;
			command := []
		)
	) else (
		match !mode with
		| Insert -> (unopt focused)#key k
		| Command ->
			let do_exec =
				if k = Term.ascii_return then true else
				if k = Term.Key.backspace then (
					if List.length !command > 0 then command := List.tl !command ;
					false
				) else (
					command := k :: !command ;
					k > 255
				) in
			if do_exec then (
				let cmd = List.rev !command in
				command := [] ;
				if !auto_insert && focused <> None then mode := Insert ;
				Cmd.execute_times cmd
			)
	)

let rec key_loop last_error =
	let left = match !mode with
		| Insert -> last_error
		| Command -> "Cmd: " ^ Cmd.string_of_command (List.rev !command)
	and right = match !mode with
		| Insert -> "Insert"
		| Command -> "Command" in
	Win.display_root left right ;

	let k = Term.key () in
	let next_error =
		try add_key k ; ""
		with Cmd.Error str -> str in
	key_loop next_error

let start =
	let instdir = Sys.getcwd () in
	Repl.repl#eval ("#load \""^instdir^"/system.cmo\"") ;
	Repl.repl#eval ("#use \""^(Unix.getenv "HOME")^".otrui.rc\"") ;
	Repl.repl#eval "#use \"./.otrui.rc\"" ;
	key_loop ""

