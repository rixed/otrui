open Bricabrac
open Curses
module Rope = Rope_impl.Make

module Log =
struct
	let ch = open_out "otrui.log"
	let p fmt = Printf.fprintf ch (fmt^^"\n%!")
end

let chk ok =
	if not ok then (
		Log.p "ncurses error!" ;
		failwith "Error"
	)

let print x y c = if not (mvaddch y x (int_of_char c)) then Log.p "Cannot print at %d %d" x y

let set_palette p f b = chk (init_pair p f b)

let char_of_key k =
	Log.p "Got key %d" k ;
	if k = Key.eol || k = 13 then '\n'
	(* TODO: handle DEL, backspace... *)
	else if k > 255 then '?'
	else char_of_int k

let init_ncurses () =
	Log.p "Initializing ncurses..." ;
	ignore (initscr ()) ;
	assert (has_colors ()) ; (* FIXME *)
	Log.p "  Starting colors..." ;
	chk (start_color ()) ;
	Log.p "  Initializing some color pairs..." ;
	for c = 1 to (colors () - 1) do set_palette c c Color.black done ;
	set_palette 2 Color.green Color.black ;
	set_palette 3 Color.blue Color.black ;
	Log.p "  cbreak..." ;
	chk (cbreak ()) ;
	Log.p "  noecho..." ;
	chk (noecho ()) ;
	Log.p "  nonl..." ;
	nonl () ;
	Log.p "  keypad..." ;
	chk (keypad (stdscr ()) true) ;
	Log.p "  hide cursor..." ;
	chk (curs_set 0)

let quit () =
	Log.p "Exiting ncurses" ;
	endwin ()

(* Buffers are somewhat editable content. *)

class virtual buffer =
object
	method virtual get : char Rope.t
	method virtual set : char Rope.t -> unit
end

class text_buffer init_content =
object
	inherit buffer
	val mutable content = init_content
	method get = content
	method set c = content <- c
	method append c = content <- Rope.cat content c
end

(* A view is a window that can be opened/closed/moved/resized
 * and which purpose is to display a buffer *)

type split_dir = Horizontal | Vertical 
type split_size = Absolute of int (* min abs size *) | Relative of float

class virtual view =
object
	method virtual display : int -> int -> int -> int -> unit
	(* Et une autre pour interpreter une commande... c'est à dire qu'on peut appeler directement
	 * une autre méthode pour modifier l'état interne de buffer... *)
	method virtual key : int -> unit
end

class text_view buf =
object
	inherit view
	val mutable color = 0	(* By default, use default terminal color pair (white on black usually) *)
	val mutable wrap_symbol_color = 0
	val mutable wrap_lines = false

	method set_wrap ?(symbol_color) w =
		wrap_lines <- w ;
		wrap_symbol_color <- optdef symbol_color color

	(* val offset_x, offset_y, wrap_lines... *)
	method display x0 y0 width height =
		let put_chr_nowrap (x, y) c =
			if y >= height then raise Exit ;
			if c = '\n' then (
				for x = x to width-1 do print (x+x0) (y+y0) ' ' done ;
				0, y+1
			) else (
				if x < width-1 then (
					print (x+x0) (y+y0) c
				) else if x = width-1 (* and not the last or next newline *) then (
					attrset (A.color_pair wrap_symbol_color) ;
					print (x+x0) (y+y0) '+' ;
					attrset (A.color_pair color)
				) ;
				x+1, y
			) in
		let rec put_chr_wrap (x, y) c =
			if y >= height then raise Exit ;
			if c = '\n' then (
				for x = x to width-1 do print (x+x0) (y+y0) ' ' done ;
				0, y+1
			) else (
				if x < width-1 then (
					print (x+x0) (y+y0) c ;
					x+1, y
				) else (
					attrset (A.color_pair wrap_symbol_color) ;
					print (x+x0) (y+y0) '\\' ;
					attrset (A.color_pair color) ;
					put_chr_wrap (0, y+1) c
				)
			) in
		attrset (A.color_pair color) ;
		let put_chr = if wrap_lines then put_chr_wrap else put_chr_nowrap in
		try ignore (Rope.fold_left put_chr (0, 0) buf#get)
		with Exit -> ()

	method key k =
		let c = char_of_key k in
		buf#append (Rope.singleton c)
end

type win =
	| Leaf of view
	| Split of (split_dir * (split_size * win) list)

let win_sub_sizes children =
	let rec aux abs rel = function
		| [] -> abs, rel
		| (Absolute a, _)::l -> aux (abs+a) rel l
		| (Relative r, _)::l -> aux abs (rel+.r) l in
	aux 0 0. children

let default_split_dir = Horizontal

let split ?(split_dir=default_split_dir) view size win =
	Split (split_dir, [ size, Leaf view ; Relative 1., win ])

let resplit view size = function
	| Leaf _ -> failwith "Cannot resplit leaf window"
	| Split (split_dir, children) ->
		Split (split_dir, (size, Leaf view) :: children)

let size_of rem_abs = function
	| Absolute a -> min a rem_abs
	| Relative r -> int_of_float ((float_of_int rem_abs) *. r)

let rec display x0 y0 width height = function
	| Leaf view -> view#display x0 y0 width height
	| Split (dir, children) ->
		let rec aux c = function
			| [] -> failwith "should not happen"
			| [_, win] ->
				let x0, y0, width, height = match dir with
				| Horizontal -> x0, c, width, height - c
				| Vertical -> c, y0, width - c, height in
				if width > 0 && height > 0 then display x0 y0 width height win
			| (sz, win) :: children' ->
				let x0, y0, width, height = match dir with
				| Horizontal -> x0, c, width, size_of (height - c) sz
				| Vertical -> c, y0, size_of (width - c) sz, height in
				if width > 0 && height > 0 then display x0 y0 width height win ;
				let next_c = match dir with Horizontal -> y0 + height | Vertical -> x0 + width in
				aux next_c children' in
		aux 0 children

let repl = new text_buffer Rope.empty

let root =
	let height, width = getmaxyx (stdscr ()) in
	Log.p "Total resolution = %d x %d" width height ;
	Log.p "Max colors = %d" (colors ()) ;
	Log.p "Max color pairs = %d" (color_pairs ()) ;
	let content = new text_buffer (Rope.of_file "test.ml") in
	let v1 = new text_view content
	and v2 = new text_view content in
	v1#set_wrap ~symbol_color:1 true ;
	v2#set_wrap ~symbol_color:1 false ;
	let top = split ~split_dir:Vertical (v2:>view) (Relative 0.5) (Leaf (v1:>view)) in
	let repl_view = new text_view repl in
	split (repl_view:>view) (Absolute 10) top

let display_root () =
	let win = stdscr () in
	let height, width = getmaxyx win in
	display 0 0 width height root

let focused =
	let rec get_first_view = function
		| [] -> failwith "No views?"
		| (_, Leaf view) :: _ -> view
		| (_, Split (_, children)) :: l -> get_first_view (children @ l) in
	get_first_view [Relative 1., root]

let ends_with e r =
	let lr = Rope.length r in
	let le = String.length e in
	if lr >= le then (
		let e' = Rope.to_string (Rope.sub r (lr-le) lr) in
		e' = e
	) else false

let repl_len = ref 0
(* toploop can write into repl independantly *)
let read_for_toploop prompt buffer buffer_len =
	let fill_buffer () =
		(* Copy into buffer as much as we can *)
		let len = Rope.length repl#get in
		let nb_items = min (len - !repl_len) buffer_len in
		let cmd = Rope.sub repl#get !repl_len (!repl_len + nb_items) in
		Log.p "Got a new command: '%s'" (Rope.to_string cmd) ;
		Rope.iteri (String.set buffer) cmd ;
		repl_len := !repl_len + nb_items ;
		nb_items, false in
	(* If we have a prompt, then we save the length after the prompt *)
	if prompt = "" && Rope.length repl#get > !repl_len then (
		Log.p "There was some content left in repl for the toplevel" ;
		fill_buffer ()
	) else (
		Log.p "Loop for getch until something is ready for the toplevel" ;
		repl#append (Rope.of_string prompt) ;
		repl_len := Rope.length repl#get ;
		(* read keys in loop, monitoring repl buffer for new content to send to toploop *)
		let rec next_key () =
			display_root () ;
			chk (refresh ()) ;
			let k = getch () in
			if k = 27 (* ESC *) then (
				quit () ;
				exit 0
			) else (
				focused#key k ;
				if k = 13 && Rope.length repl#get > !repl_len && ends_with ";;\n" repl#get then
					fill_buffer ()
				else next_key ()
			) in
		next_key ()
	)

let start_loop =
	init_ncurses () ;
	(* Input for toplevel must be talen from our special toplevel buf *)
	Toploop.read_interactive_input := read_for_toploop ;
	(* And output from toplevel should be redirected into this buf *)
	let rope_formatter buf =
		let out str pos len =
			let s = String.sub str pos len in
			let r = Rope.of_string s in
			Log.p "Toplevel formatting output '%s'" s ;
			buf#append r in
		Format.make_formatter out nop in
	let toploop_formatter = rope_formatter repl in
	(* Load otrui *)
	Toploop.loop toploop_formatter

