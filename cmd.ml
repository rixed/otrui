open Bricabrac
module Rope = Buf.Rope

(* Helpers *)

(* This function requires both views and wins so cannot be implemented in view module. *)
let views () =
	let rec show_texts prevs = function
		| [] -> prevs
		| txt :: rest ->
			let buf = txt#get_buf#name in
			let is_mapped = Win.is_mapped (txt:>View.t) in
			let is_focused = Win.is_focused (txt:>View.t) in	
			let descr = (txt :> View.t), "text", buf, is_mapped, is_focused in
			show_texts (descr :: prevs) rest in
	show_texts [] !View.text_views

let string_of_view (_, kind, buf, is_mapped, is_focused) =
	Printf.sprintf "%s%s: %s"
		(if is_focused then "*" else if is_mapped then " " else "_")
		kind buf

(* Command parse and execute *)

let last_result = ref ""

let c2i = int_of_char

let way_of_key dir =
	if dir = Term.Key.up    then Win.Up else
	if dir = Term.Key.down  then Win.Down else
	if dir = Term.Key.left  then Win.Left else
	if dir = Term.Key.right then Win.Right else
	invalid_arg "not a direction"

let execute_single = function
	(* quit *)
	| [ q ] when q = c2i 'q' ->
		Log.p "Quit" ;
		Term.quit () ; exit 0
	(* change focus *)
	| [ w ; dir ] when w = c2i 'w' && Term.is_direction dir ->
		Log.p "Changing focus" ;
		(try
			let way = way_of_key dir in
			Win.move_focus_to way
		with Not_found ->
			last_result := "No window there")
	| [ w ; npage ] when w = c2i 'w' && npage = Term.Key.npage ->
		(try Win.deepen_focus ()
		with Not_found ->
			last_result := "No window down there")
	| [ w ; ppage ] when w = c2i 'w' && ppage = Term.Key.ppage ->
		(try Win.widen_focus ()
		with Not_found ->
			last_result := "No window up there")
	(* change window size *)
	| [ w ; a ; dir ] when w = c2i 'w' && (a = c2i '+' || a = c2i '-') && Term.is_direction dir ->
		let way = way_of_key dir in
		(try
			let sz = if a = c2i '+' then 1 else ~-1 in
			Win.resize_focus way sz
		with Not_found ->
			last_result := "Cannot resize in this direction")
	(* Exchange two windows *)
	| [ w ; x ; dir ] when w = c2i 'w' && x = c2i 'x' && Term.is_direction dir ->
		let way = way_of_key dir in
		(try Win.exchange_focus way
		with Not_found ->
			last_result := "No other window in this direction")
	(* Unmap the focused view *)
	| [ w ; d ] when w = c2i 'w' && d = c2i 'd' ->
		(try Win.delete_focus ()
		with Not_found ->
			last_result := "Cannot unmap everything")
	(* Change the view of the focused window to the next one *)
	| [ b ; n ] when b = c2i 'b' && n = c2i 'n' ->
		(try
			let views = views () in
			let first = List.hd views in
			let rec aux take_next = function
				| [] -> assert take_next ; first
				| (v, _, _, _, _) as vdescr :: rest ->
					if take_next then vdescr else aux (Win.is_focused v) rest in
			(match aux false views with (v, _, bufname, _, _) ->
				Win.set_view v ;
				last_result := Printf.sprintf "Viewing '%s'" bufname)
		with Not_found ->
			last_result := "Not a single views?")
	(* Change the view of the focused window to the previous one *)
	| [ b ; p ] when b = c2i 'b' && p = c2i 'p' ->
		(try
			let views = views () in
			let rec aux prev = function
				| [] -> prev (* the first was focused *)
				| (v, _, _, _, _) as vdescr :: rest ->
					if Win.is_focused v then prev else aux vdescr rest in
			(match aux (List.hd views) (List.tl views) with (v, _, bufname, _, _) ->
				Win.set_view v ;
				last_result := Printf.sprintf "Viewing '%s'" bufname)
		with Not_found ->
			last_result := "Not a single views?")
	(* Split the current focused window *)
	| [ w ; s ; dir ] when w = c2i 'w' && s = c2i 's' && Term.is_direction dir ->
		(try Win.split_focus (way_of_key dir)
		with _ -> last_result := "Cannot split?")
	(* send a command to the repl *)
	| bang :: cmd when bang = c2i '#' ->
		(try
			let cmd = List.map char_of_int cmd in
			let cmd = Rope.of_list cmd in
			let cmd = Rope.to_string cmd in (* ouf! *)
			Buf.repl#eval cmd
		with Invalid_argument _ ->
			last_result := "Cannot exec this 'string'")
	(* and '!' to send a command to a shell, opening a new shell view if none already opened ?
	 * So we need the "pipe" buf *)
	(* TODO? Non car le principe c'est qu'il soit possible de coder ce genre de fonctionnalité
	 * depuis l'éditeur lui même... *)
	(* De même, coder un module qui, si on le charge au démarrage, va relire la configuration
	 * de la win root depuis un fichier (s'il existe), et l'enregistrer au moment de quitter.
	 * (il faudra alors penser à ajouter des hooks) *)
	(* unrecognized command *)
	| _ -> last_result := "Unknown command"

let rec execute ?count = function
	(* nop *)
	| [] -> last_result := ""
	(* repetition count *)
	| c :: rest when c >= c2i '0' && c <= c2i '9' ->
		execute ~count:((optdef count 0)*10 + c - (c2i '0')) rest
	| cmd ->
		for c = 1 to (optdef count 1) do execute_single cmd done

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
		let focused = try Some (Win.view_of !Win.root) with Not_found -> None in
		if focused = None then mode := Command ;
		let left = match !mode with
			| Insert -> !last_result
			| Command -> "Cmd: " ^ string_of_command !command
		and right = match !mode with
			| Insert -> "Insert"
			| Command -> "Command" in
		Win.display_root left right ;
		let k = Term.key () in
		if k = Term.ascii_escape then (
			if focused <> None then (
				mode := (match !mode with Command -> Insert | Insert -> Command) ;
				last_result := "" ;
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
					execute (List.rev !command) ;
					command := [] ;
					if !auto_insert && focused <> None then mode := Insert
				)
		) in
	forever next_key ()

