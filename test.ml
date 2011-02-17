(* ------------------------------------------------------------------------------ *)
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

let init () =
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

let exit () =
	Log.p "Exiting ncurses" ;
	endwin ()

type split_dir = Horizontal | Vertical 
type split_size = Absolute of int (* min abs size *) | Relative of float

class virtual buf =
object
	method virtual display : int -> int -> int -> int -> unit
	(* Et une autre pour interpreter une commande... c'est à dire qu'on peut appeler directement
	 * une autre méthode pour modifier l'état interne de buffer... *)
	method virtual key : int -> unit
end

class unbound_buf =
object
	inherit buf
	method display _x0 _y0 _width _height = ()
	method key _k = ()
end

class text_buf text =
object
	inherit buf
	val mutable text = text
	method set_text t = text <- t 	(* TODO: save previous versions for undo *)
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
		try ignore (Rope.fold_left put_chr (0, 0) text)
		with Exit -> ()

	method key _k = ()
end

class pipe_buf ch_in ch_out =
	let text = Rope.empty in
object (self)
	inherit text_buf text
	val ch_in = ch_in
	val ch_out = ch_out
	val mutable last_len = 0
	method key k =
		let c = char_of_key k in
		self#set_text (Rope.cat text (Rope.singleton c)) ;
		if c = '\n' then (
			let len = Rope.length text in
			let last_cmd = Rope.sub text last_len len in
			last_len <- len ;
			output_string ch_out (Rope.to_string last_cmd) ;
			flush ch_out ;
			let resp = input_line ch_in in
			self#set_text (Rope.cat text (Rope.of_string resp))
		)
end

class cmd_buf submit_cmd =
	let text = Rope.empty in
object (self)
	inherit text_buf text
	val mutable last_len = 0
	method key k =
		let c = char_of_key k in
		self#set_text (Rope.cat text (Rope.singleton c)) ;
		if c = '\n' then (
			let len = Rope.length text in
			let last_cmd = Rope.sub text last_len len in
			let result = submit_cmd last_cmd in
			self#set_text (Rope.cat text result) ;
			last_len <- Rope.length text ;
		)
end


type win =
	| Leaf of buf
	| Split of (split_dir * (split_size * win) list)

let win_sub_sizes children =
	let rec aux abs rel = function
		| [] -> abs, rel
		| (Absolute a, _)::l -> aux (abs+a) rel l
		| (Relative r, _)::l -> aux abs (rel+.r) l in
	aux 0 0. children

let default_split_dir = Horizontal

let split ?(split_dir=default_split_dir) buf size win =
	Split (split_dir, [ size, Leaf buf ; Relative 1., win ])

let resplit buf size = function
	| Leaf _ -> failwith "Cannot resplit leaf window"
	| Split (split_dir, children) ->
		Split (split_dir, (size, Leaf buf) :: children)

let size_of rem_abs = function
	| Absolute a -> min a rem_abs
	| Relative r -> int_of_float ((float_of_int rem_abs) *. r)

let rec display x0 y0 width height = function
	| Leaf buf -> buf#display x0 y0 width height
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

let display_root root =
	let win = stdscr () in
	let height, width = getmaxyx win in
	display 0 0 width height root

let _ =
	Toploop.initialize_toplevel_env()

let rope_formatter rope_ref =
	let out str pos len =
		let s = String.sub str pos len in
		let r = Rope.of_string s in
		rope_ref := Rope.cat !rope_ref r in
	Format.make_formatter out nop

let submit_toplevel q =
	let query = Rope.to_string q in
	let lb = Lexing.from_string query in
	let phr = !Toploop.parse_toplevel_phrase lb in
	let rope_ref = ref Rope.empty in
	let ok = Toploop.execute_phrase true (rope_formatter rope_ref) phr in
	Log.p "Toplevel: %s -> %s (%b)" query (Rope.to_string !rope_ref) ok ;
	!rope_ref

let test () =
	let height, width = getmaxyx (stdscr ()) in
	Log.p "Total resolution = %d x %d" width height ;
	Log.p "Max colors = %d" (colors ()) ;
	Log.p "Max color pairs = %d" (color_pairs ()) ;
	let content = Rope.of_file "test.ml" in
	let b1 = new text_buf content
	and b2 = new text_buf content in
	b1#set_wrap ~symbol_color:1 true ;
	b2#set_wrap ~symbol_color:1 false ;
	let top = split ~split_dir:Vertical (b2:>buf) (Relative 0.5) (Leaf (b1:>buf)) in
	let repl = new cmd_buf submit_toplevel in
	let root = split (repl:>buf) (Absolute 10) top in
	(* read keys in loop *)
	let focused = repl in
	let rec next_key () =
		display_root root ;
		chk (refresh ()) ;
		let k = getch () in
		if k = 27 (* ESC *) then (
			exit ();
		) else (
			focused#key k ;
			next_key ()
		) in
	next_key ()

let main =
	init () ;
	(* Do not write into repl_in until the toplevel is actually reading, or we will deadblock.
	 * The solution here is to start a new thread *)
	try_finalize test () exit ()
