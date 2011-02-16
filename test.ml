(* ------------------------------------------------------------------------------ *)
open Bricabrac
open Curses
module Rope = Rope_impl.Make

let errs = ref []
let chk ok =
	if not ok then (
		errs := (Printf.sprintf "Error...") :: !errs ;
		failwith "Error"
	)

let print x y c = if not (mvaddch y x (int_of_char c)) then errs := (Printf.sprintf "Cannot print at %d %d" x y)::!errs

let set_palette p f b = chk (init_pair p f b)

let init () =
	ignore (initscr ()) ;
	assert (has_colors ()) ; (* FIXME *)
	chk (start_color ()) ;
	for c = 1 to (colors () - 1) do set_palette c c Color.black done ;
	set_palette 2 Color.green Color.black ;
	set_palette 3 Color.blue Color.black ;
	chk (cbreak ()) ;
	chk (noecho ()) ;
	chk (curs_set 0)

let exit () =
	endwin ()

type split_dir = Horizontal | Vertical 
type split_size = Absolute of int (* min abs size *) | Relative of float

class virtual buf =
object
	method virtual display : int -> int -> int -> int -> unit
	(* Et une autre pour interpreter une commande... c'est à dire qu'on peut appeler directement
	 * une autre méthode pour modifier l'état interne de buffer... *)
end

class unbound_buf =
object
	inherit buf
	method display _x0 _y0 _width _height = ()
end

class text_buf text =
object
	inherit buf
	val text = text
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

let split ?(split_dir) buf size = function
	| Leaf _ as prev ->
		Split (optdef split_dir default_split_dir, [ size, Leaf buf ; Relative 1., prev ])
	| Split (prev_split_dir, children) ->
		may split_dir (fun dir ->
			if prev_split_dir <> dir then failwith "Cannot resplit in another direction") ;
		Split (prev_split_dir, (size, Leaf buf) :: children)

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
	(* FIXME: add_msg "fmt" x y *)
	errs := (Printf.sprintf "Total resolution = %d x %d" width height) :: !errs ;
	errs := (Printf.sprintf "Max colors = %d" (colors ())) :: !errs ;
	errs := (Printf.sprintf "Max color pairs = %d" (color_pairs ())) :: !errs ;
	display 0 0 width height root

let test () =
	let content = Rope.of_file "test.ml" in
	let b1 = new text_buf content
	and b2 = new text_buf content in
	b1#set_wrap ~symbol_color:1 true ;
	b2#set_wrap ~symbol_color:1 false ;
	let root = split ~split_dir:Vertical (b2:>buf) (Relative 0.5) (Leaf (b1:>buf)) in
	display_root root ;
	chk (refresh ()) ;
	(* Then wait a key *)
	ignore (getch ())

let main =
	init () ;
	ignore_exceptions test () ;
	exit () ;
	List.iter (fun s -> Printf.printf "%s\n" s) !errs
