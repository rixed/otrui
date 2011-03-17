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

let repl_view = Repl_view.view (Repl_text_view.create ~append:true repl) "REPL"

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
let hashtbl_keys h = Hashtbl.fold (fun k _ l -> k::l) h []
let hashtbl_values h = Hashtbl.fold (fun _ v l -> v::l) h []
let view_names () = hashtbl_keys named_views
let views () = hashtbl_values named_views

(* Another test *)

let rope_buf_of_file fname =
	let r = Buf.create () in
	Buf.append r (Rope.of_file fname) ;
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
	let view = unopt (Win.root (Win.to_leaf !win)) in
	win := Win.deepen (Win.split dir !win view)

(* Key management *)

type mode = Command | Insert
let mode = ref Insert
let auto_insert = ref true	(* return in insert mode once command is executed *)
let command = ref []
let last_error = ref ""
let mutex = Mutex.create ()
let redraw_cond = Condition.create ()

let init_default_commands =
	let way_of_key dir =
		if dir = Term.Key.up    then Win.Up else
		if dir = Term.Key.down  then Win.Down else
		if dir = Term.Key.left  then Win.Left else
		if dir = Term.Key.right then Win.Right else
		invalid_arg "not a direction" in
	let is_direction k =
		k = Term.Key.left || k = Term.Key.right ||
		k = Term.Key.up   || k = Term.Key.down in
	let rec get_next_unmapped views =
		let rec aux take = function
			| [] -> aux true views
			| v :: rest ->
				if take && is_focused v then raise Not_found ;
				if take && not (is_mapped v) then v
				else aux (take || is_focused v) rest in
		aux false views in
	let c2i = Cmd.c2i
	and prev_execute = !Cmd.execute in
	Cmd.execute := function
		(* quit *)
		| [ q ] when q = c2i 'q' ->
			Log.p "Quit" ;
			Term.quit () ; exit 0
		(* send a command to the repl *)
		| dash :: cmd when dash = c2i '#' ->
			(try Repl.eval repl (Cmd.string_of_command cmd)
			with Invalid_argument _ -> Cmd.error "Cannot evaluate this 'string'")
		(* change focus *)
		| [ w ; dir ] when w = c2i 'w' && is_direction dir ->
			Log.p "Changing focus" ;
			(try
				let way = way_of_key dir in
				move_focus_to way
			with Not_found -> Cmd.error "No window there")
		| [ w ; npage ] when w = c2i 'w' && npage = Term.Key.npage ->
			(try deepen_focus ()
			with Not_found -> Cmd.error "No window down there")
		| [ w ; ppage ] when w = c2i 'w' && ppage = Term.Key.ppage ->
			(try widen_focus ()
			with Not_found -> Cmd.error "No window up there")
		(* change window size *)
		| [ w ; a ; dir ] when w = c2i 'w' && (a = c2i '+' || a = c2i '-') && is_direction dir ->
			let way = way_of_key dir in
			(try
				let sz = if a = c2i '+' then 1 else ~-1 in
				resize_focus way sz
			with Not_found -> Cmd.error "Cannot resize in this direction")
		(* Exchange two windows *)
		| [ w ; x ; dir ] when w = c2i 'w' && x = c2i 'x' && is_direction dir ->
			let way = way_of_key dir in
			(try exchange_focus way
			with Not_found -> Cmd.error "No other window in this direction")
		(* Unmap the focused view *)
		| [ w ; d ] when w = c2i 'w' && d = c2i 'd' ->
			(try delete_focus ()
			with Not_found -> Cmd.error "Cannot unmap everything")
		(* Change the view of the focused window to the next hidden one.
		 * This is important that a view is not mapped several times, since a view
		 * is supposed to be unique (for instance, text_view, store it's size, cursor position, etc.) *)
		| [ b ; n ] when b = c2i 'b' && n = c2i 'n' ->
			(try set_view (get_next_unmapped (views ()))
			with Not_found -> Cmd.error "No hidden views")
		(* Change the view of the focused window to the previous one *)
		| [ b ; p ] when b = c2i 'b' && p = c2i 'p' ->
			(try set_view (get_next_unmapped (List.rev (views ())))
			with Not_found -> Cmd.error "No hidden views")
		(* Split the current focused window *)
		| [ w ; s ; dir ] when w = c2i 'w' && s = c2i 's' && is_direction dir ->
			(try split_focus (way_of_key dir)
			with _ -> Cmd.error "Cannot split?")
		(* unknown command *)
		| x -> prev_execute x


let rec key_loop () =
	let rec do_count_times focused ?count = function
		(* nop *)
		| [] -> ()
		(* repetition count *)
		| c :: rest when c >= Cmd.c2i '0' && c <= Cmd.c2i '9' ->
			do_count_times focused ~count:((optdef count 0)*10 + c - (Cmd.c2i '0')) rest
		| cmd ->
			for c = 1 to (optdef count 1) do
				try may focused (fun view -> view.execute cmd)
				with Cmd.Unknown -> !Cmd.execute cmd
			done in
	let handle_key k =
		Log.p "Got key %d" k ;
		let focused = Win.root !win in
		if focused = None then mode := Command ;

		if k = Term.Key.escape then (
			if focused <> None then (
				mode := (match !mode with Command -> Insert | Insert -> Command) ;
				command := []
			)
		) else (
			match !mode with
			| Insert -> (unopt focused).key k
			| Command ->
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
					Log.p "Executing cmd %s" (Cmd.string_of_command cmd) ;
					command := [] ;
					if !auto_insert && focused <> None then mode := Insert ;
					do_count_times focused cmd
				)
		) in
	let k = Term.Key.get () in
	Mutex.lock mutex ;
	last_error :=
		(try handle_key k ; ""
		with Cmd.Error str -> str) ;
	Condition.signal redraw_cond ;
	Mutex.unlock mutex ;
	key_loop ()

(* The thread that redraw the screen when signaled *)

let draw_thread () =
	let loop () =
		Mutex.lock mutex ;
		let left = match !mode with
			| Insert -> !last_error
			| Command -> "Cmd: " ^ Cmd.string_of_command (List.rev !command)
		and right = match !mode with
			| Insert -> "Insert"
			| Command -> "Command" in
		draw !win left right ;
		Condition.wait redraw_cond mutex ;
		Mutex.unlock mutex in
	forever loop ()

let start () =
	ignore (Thread.create draw_thread ()) ;
	key_loop ()

