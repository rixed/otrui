open Bricabrac
module Rope = Buf.Rope

let focused = ref (Win.first_leaf !Win.root)

let last_result = ref ""

let execute = function
	(* nop *)
	| [] -> last_result := ""
	(* quit *)
	| [ q ] when q = int_of_char 'q' ->
		Log.p "Quit" ;
		Term.quit () ; exit 0
	(* change focus *)
	| [ w ; dir ] when w = int_of_char 'w' && Term.is_direction dir ->
		Log.p "Changing focus" ;
		(try
			     if dir = Term.Key.up    then focused := Win.next_to !focused Win.Horizontal ~-1
			else if dir = Term.Key.down  then focused := Win.next_to !focused Win.Horizontal 1
			else if dir = Term.Key.left  then focused := Win.next_to !focused Win.Vertical   ~-1
			else if dir = Term.Key.right then focused := Win.next_to !focused Win.Vertical   1
		with Not_found ->
			last_result := "No view there")
	(* send a command to the repl *)
	| bang :: cmd when bang = int_of_char '!' ->
		(try
			let cmd = List.map char_of_int cmd in
			let cmd = Rope.of_list cmd in
			let cmd = Rope.cat cmd (Rope.of_string ";;\n") in
			Buf.repl#append cmd
		with Invalid_argument _ ->
			last_result := "Cannot exec this 'string'")
	(* unrecognized command *)
	| _ -> last_result := "Unknown command"

let string_of_command command =
	let len = List.length command in
	let str = String.create len in
	let rec aux i cmds =
		if i >= 0 then (
			str.[i] <- (try char_of_int (List.hd cmds) with Invalid_argument _ -> '?') ;
			aux (i-1) (List.tl cmds)
		) in
	aux (len-1) command ;
	str

type mode = Command | Insert
let mode = ref Insert
let command = ref []
let auto_insert = ref true	(* return in insert mode once command is executed *)
let key_loop () =
	Log.p "Loop for getch" ;
	let next_key () =
		let left = match !mode with
			| Insert -> !last_result
			| Command -> "Cmd: " ^ string_of_command !command
		and right = match !mode with
			| Insert -> "Insert"
			| Command -> "Command" in
		Win.display_root left right ;
		let k = Term.key () in
		if k = Term.ascii_escape then (
			mode := (match !mode with Command -> Insert | Insert -> Command) ;
			last_result := "" ;
			command := []
		) else (
			match !mode with
			| Insert -> (Win.view_of !focused)#key k
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
					execute (List.rev !command) ;
					command := [] ;
					if !auto_insert then mode := Insert
				)
		) in
	forever next_key ()

let init () =
	focused := Win.first_leaf !Win.root
