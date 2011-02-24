open Bricabrac

let focused =
	let rec get_first_view = function
		| [] -> failwith "No views?"
		| (_, Win.Leaf view) :: _ -> view
		| (_, Win.Split (_, children)) :: l -> get_first_view (children @ l) in
	get_first_view [Win.Relative 1., Win.root]

let last_result = ref ""

let execute = function
	| [] -> last_result := ""
	| [ single ] when single = int_of_char 'q' -> Term.quit () ; exit 0
	| _ -> last_result := "Unknown command"

let string_of_command command =
	let len = List.length command in
	let str = String.create len in
	let rec aux i cmds =
		if i >= 0 then (
			str.[i] <- char_of_int (List.hd cmds) ;
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
			mode := match !mode with Command -> Insert | Insert -> Command
		) else (
			match !mode with
			| Insert -> focused#key k
			| Command ->
				if k = Term.ascii_return then (
					execute (List.rev !command) ;
					command := [] ;
					if !auto_insert then mode := Insert
				) else (
					command := k :: !command
				)
		) in
	forever next_key ()
