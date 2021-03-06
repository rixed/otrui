open Bricabrac
open Otrui

(* Some modules *)

module Term = Term_impl.Make (Term_curses.Make)
module Cmd = Cmd_impl.Make (Term)
module Buf = Buf_impl.Make (Cmd)
module Repl = Buf_repl.Make (Buf)

module Repl_text_view = View_text.Make (Repl) (Term) (Cmd)
module Repl_view = View_impl.Make (Repl_text_view)
(* FIXME: replace by a true File_text_view, with the File buffer maps to a given file *)
module Rope_text_view = View_text.Make (Buf) (Term) (Cmd)
module Rope_view = View_impl.Make (Rope_text_view)

(* At the beginning there was a REPL *)

let repl =
	let r = Repl.create () in
	Repl.grab_topdirs r ;
	r

(* Then a text editor for editing it *)

let repl_view =
	let r = Repl_text_view.create ~append:true repl in
	Hashtbl.add r.Repl_text_view.colors (Rope.anot_of_string "prompt") (Term.get_color (100, 100, 1000) (0, 0, 0)) ;
	Repl_view.view r "REPL"

(* An empty window to show it *)

module Win = Win_impl.Make

let win = ref (Win.singleton repl_view)

(* Then the set of named views that must be kept even when not in any window *)

let named_views = Hashtbl.create 17
let () = Hashtbl.add named_views "REPL" repl_view

let add_view name view =
	let rec uniq_name suffix =
		let n = (if suffix > 1 then string_of_int suffix else "") ^ name in
		if Hashtbl.mem named_views n then uniq_name (suffix+1) else n in
	let n = uniq_name 1 in
	Hashtbl.add named_views n view

let add_and_open_view name v =
	add_view name v ;
	win := Win.deepen (Win.split Win.Up !win v)

let del_view name = Hashtbl.remove named_views name

let get_view name = Hashtbl.find named_views name

(* Returns a list of all kept views *)
let view_names () = hashtbl_keys named_views
let views () = hashtbl_values named_views

(* Another test *)

let rope_buf_of_file fname =
	let r = Buf.create () in
	Buf.append r (Rope.make_unknown (Rope.of_file fname)) ;
	Buf.reset_undo r ;
	r
let add_and_open_file fname =
	let view = Rope_text_view.create ~append:true (rope_buf_of_file fname) in
	add_and_open_view fname (Rope_view.view view fname)

let () = add_and_open_file ".otrui.rc"

(* And then the function to draw the window *)

let global_status_color  = ref (Term.get_color (0, 0, 0) (800, 800, 800))
let win_status_color     = ref (Term.get_color (0, 0, 0) (900, 900, 900))
let focused_status_color = ref (Term.get_color (1000, 1000, 1000) (300, 300, 500))
let vert_split_color     = ref (Term.get_color (0, 0, 0) (900, 900, 900))
let show_vert_split      = ref true

let draw win status_left status_right =
	let draw_status left right color x0 y0 width =
		let descr_len  = String.length left
		and status_len = String.length right in
		Term.set_color color ;
		for i = 0 to width - 1 do Term.print (x0+i) y0 Term.hline done ;
		if width >= status_len then (
			Term.print_string (x0+width-status_len) y0 right ;
			let width = width - status_len - 1 in
			if width >= descr_len then
				Term.print_string x0 y0 left
			else
				Term.print_string x0 y0 (String.sub left 0 width)
		) in
	let draw_vert_split x0 y0 height =
		Term.set_color !vert_split_color ;
		for y = 0 to (height-1) do Term.print x0 (y0+y) Term.vline done in
	let draw_view x0 y0 width height is_focused view =
		assert (height >= 1) ;
		let descr, status = view.descr (), view.status () in
		let need_status = height > 1 && (descr <> "" || status <> "") in
		let need_vert_split = !show_vert_split && width > 1 in
		view.draw x0 y0 (if need_vert_split then width-1 else width) (if need_status then height-1 else height) is_focused ;
		if need_vert_split then draw_vert_split (x0+width-1) y0 height ;
		if need_status then (
			let color = if is_focused then !focused_status_color else !win_status_color in
			draw_status descr status color x0 (y0+height-1) width
		) in
	let width, height = Term.screen_size () in
	if height > 1 then Win.iter draw_view 0 0 width (height-1) win ;
	draw_status status_left status_right !global_status_color 0 (height-1) width ;
	Term.redraw ()

(* Helpers to manipulate win (used in following commands) *)

let move_focus_to way   = win := Win.to_leaf (Win.focus_to way !win)
let deepen_focus ()     = win := Win.deepen !win
let widen_focus ()      = win := Win.widen !win
let resize_focus way sz = win := Win.resize way sz !win
let exchange_focus way  = win := Win.exchange way !win
let delete_focus ()     = win := Win.to_leaf (Win.delete !win)
let is_focused view     = match Win.root !win with Some v when v == view -> true | _ -> false
let is_mapped view      = Win.exists ((==) view) !win
let set_view view       = win := Win.set_root !win view
let split_focus dir     =
	(* FIXME: must not map twice the same view. Build a new empty view or take the next unmaped? *)
	let view = unopt (Win.root (Win.to_leaf !win)) in
	win := Win.deepen (Win.split dir !win view)

(* Key management *)

exception DialogAbort
type mode =
	  Command (* exec the first matching cmd *)
	| Dialog of (string * (int list -> unit)) (* Prompt for input *)
	| Insert
	| DisplayError of string
let mode = ref Insert
let auto_insert = ref true	(* return in insert mode once a command is executed *)
let command = ref []
let mutex = Mutex.create ()
let redraw_cond = Condition.create ()

let init_default_commands =
	let rec get_next_unmapped views =
		let rec aux take = function
			| [] -> aux true views
			| v :: rest ->
				if take && is_focused v then raise Not_found ;
				if take && not (is_mapped v) then v
				else aux (take || is_focused v) rest in
		aux false views in
	let c2i = Cmd.c2i in
	(* quit *)
	Cmd.register [ c2i 'q' ] (fun _ ->
		Log.p "Quit" ;
		Term.quit () ;
		exit 0) ;
	(* change focus *)
	Cmd.register [ c2i 'w' ; Term.Key.left  ] (fun _ -> move_focus_to Win.Left) ;
	Cmd.register [ c2i 'w' ; Term.Key.right ] (fun _ -> move_focus_to Win.Right) ;
	Cmd.register [ c2i 'w' ; Term.Key.up    ] (fun _ -> move_focus_to Win.Up) ;
	Cmd.register [ c2i 'w' ; Term.Key.down  ] (fun _ -> move_focus_to Win.Down) ;
	Cmd.register [ c2i 'w' ; Term.Key.npage ] (fun _ -> deepen_focus ()) ;
	Cmd.register [ c2i 'w' ; Term.Key.ppage ] (fun _ -> widen_focus ()) ;
	(* change window size *)
	Cmd.register [ c2i 'w' ; c2i '+' ; Term.Key.left  ] (fun _ -> resize_focus Win.Left 1) ;
	Cmd.register [ c2i 'w' ; c2i '+' ; Term.Key.right ] (fun _ -> resize_focus Win.Right 1) ;
	Cmd.register [ c2i 'w' ; c2i '+' ; Term.Key.up    ] (fun _ -> resize_focus Win.Up 1) ;
	Cmd.register [ c2i 'w' ; c2i '+' ; Term.Key.down  ] (fun _ -> resize_focus Win.Down 1) ;
	Cmd.register [ c2i 'w' ; c2i '-' ; Term.Key.left  ] (fun _ -> resize_focus Win.Left ~-1) ;
	Cmd.register [ c2i 'w' ; c2i '-' ; Term.Key.right ] (fun _ -> resize_focus Win.Right ~-1) ;
	Cmd.register [ c2i 'w' ; c2i '-' ; Term.Key.up    ] (fun _ -> resize_focus Win.Up ~-1) ;
	Cmd.register [ c2i 'w' ; c2i '-' ; Term.Key.down  ] (fun _ -> resize_focus Win.Down ~-1) ;
	(* exchange two windows *)
	Cmd.register [ c2i 'w' ; c2i 'x' ; Term.Key.left  ] (fun _ -> exchange_focus Win.Left) ;
	Cmd.register [ c2i 'w' ; c2i 'x' ; Term.Key.right ] (fun _ -> exchange_focus Win.Right) ;
	Cmd.register [ c2i 'w' ; c2i 'x' ; Term.Key.up    ] (fun _ -> exchange_focus Win.Up) ;
	Cmd.register [ c2i 'w' ; c2i 'x' ; Term.Key.down  ] (fun _ -> exchange_focus Win.Down) ;
	(* unmap the focused view *)
	Cmd.register [ c2i 'w' ; c2i 'd' ] (fun _ -> delete_focus ()) ;
	(* Change the view of the focused window to the next hidden one.
	 * This is important that a view is not mapped several times, since a view
	 * is supposed to be unique (for instance, text_view, store it's size, cursor position, etc.) *)
	Cmd.register [ c2i 'b' ; c2i 'n' ] (fun _ -> set_view (get_next_unmapped (views ()))) ;
	(* change the view of the focused window to the previous one *)
	Cmd.register [ c2i 'b' ; c2i 'p' ] (fun _ -> set_view (get_next_unmapped (List.rev (views ())))) ;
	(* split the current focused window *)
	Cmd.register [ c2i 'w' ; c2i 's' ; Term.Key.left  ] (fun _ -> split_focus Win.Left) ;
	Cmd.register [ c2i 'w' ; c2i 's' ; Term.Key.right ] (fun _ -> split_focus Win.Right) ;
	Cmd.register [ c2i 'w' ; c2i 's' ; Term.Key.up    ] (fun _ -> split_focus Win.Up) ;
	Cmd.register [ c2i 'w' ; c2i 's' ; Term.Key.down  ] (fun _ -> split_focus Win.Down) ;
	(* undo / redo *)
	Cmd.register (List.map c2i ['u';'n';'d';'o']) (fun _ ->
		may !Rope_text_view.current (fun v -> Rope_text_view.Buf.undo v.Rope_text_view.buf) ;
		may !Repl_text_view.current (fun v -> Repl_text_view.Buf.undo v.Repl_text_view.buf)) ;
	Cmd.register (List.map c2i ['r';'e';'d';'o']) (fun _ ->
		may !Rope_text_view.current (fun v -> Rope_text_view.Buf.redo v.Rope_text_view.buf) ;
		may !Repl_text_view.current (fun v -> Repl_text_view.Buf.redo v.Repl_text_view.buf)) ;
	(* delete line *)
	Cmd.register [ c2i 'd' ; c2i 'd' ] (fun _ -> may !Rope_text_view.current (fun v -> Rope_text_view.delete_line v v.Rope_text_view.cursor)) ;
	(* evaluate ocaml expression *)
	let repl_eval cmd = Repl.eval repl (Cmd.to_string cmd) in
	Cmd.register [ Cmd.c2i '#' ] (fun _ -> mode := Dialog ("Expression", repl_eval))

let rec key_loop () =
	let rec do_count_times focused ?count = function
		(* nop *)
		| [] -> ()
		(* repetition count *)
		| c :: rest when c >= Cmd.c2i '0' && c <= Cmd.c2i '9' ->
			do_count_times focused ~count:((optdef count 0)*10 + c - (Cmd.c2i '0')) rest
		| cmd ->
			let f = Cmd.to_function cmd in
			for c = 1 to (optdef count 1) do f () done in
	let handle_key k =
		Log.p "Got key %d" k ;
		let focused = Win.root !win in
		if focused = None then mode := Command ;

		if k = Term.Key.escape then (
			if focused <> None then (
				(match !mode with
					| Command  -> mode := Insert
					| Insert   -> mode := Command
					| Dialog _ -> raise DialogAbort
					| DisplayError _ -> mode := if !auto_insert then Insert else Command) ;
				command := []
			)
		) else (
			match !mode with
			| DisplayError _ -> mode := DisplayError "So what?"
			| Insert -> (unopt focused).key k
			| Dialog (_, f) ->
				Log.p "In dialog mode" ;
				let do_exec =
					if k = Term.Key.return then true else
					if k = Term.Key.backspace then (
						if List.length !command > 0 then command := List.tl !command ;
						false
					) else (
						command := k :: !command ;
						k > 255
					) in
				if do_exec then (
					let cmd = List.rev !command in
					Log.p "Dialog entry is '%s'" (Cmd.to_string cmd) ;
					command := [] ;
					if !auto_insert && focused <> None then mode := Insert else mode := Command ;
					f cmd
				)
			| Command ->
				command := k :: !command ;
				if !command <> [] then (
					let cmd = List.rev !command in
					Log.p "Executing cmd '%s'" (Cmd.to_string cmd) ;
					try
						do_count_times focused cmd ;	(* may change mode *)
						command := [] ;
						if !auto_insert && focused <> None && !mode = Command then mode := Insert
					with Cmd.Ambiguous -> ()
				)
		) in
	let k = Term.Key.get () in
	Mutex.lock mutex ;
	(try handle_key k
	with exn ->
		let buf = Buffer.create 50 in
		let fmt = Format.formatter_of_buffer buf in
		Toploop.print_exception_outcome fmt exn ;
		command := [] ;
		mode := DisplayError (Buffer.contents buf)) ;
	Condition.signal redraw_cond ;
	Mutex.unlock mutex ;
	key_loop ()

(* The thread that redraw the screen when signaled *)

let draw_thread () =
	let to_single_line str = String.escaped (string_translate str "\n" " ") in
	let loop () =
		Mutex.lock mutex ;
		let left = match !mode with
			| DisplayError str -> to_single_line str
			| Insert -> ""
			| Command -> "Cmd: " ^ Cmd.to_string (List.rev !command)
			| Dialog (p, _) -> p ^ ": " ^ Cmd.to_string (List.rev !command)
		and right = match !mode with
			| DisplayError _ -> "!"
			| Insert -> "Insert"
			| Command -> "Command"
			| Dialog _ -> "?" in
		draw !win left right ;
		Condition.wait redraw_cond mutex ;
		Mutex.unlock mutex in
	forever loop ()

let start () =
	ignore (Thread.create draw_thread ()) ;
	key_loop ()

