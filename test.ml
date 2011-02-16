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

let init () =
	ignore (initscr ()) ;
	assert (has_colors ()) ; (* FIXME *)
	chk (start_color ()) ;

	chk (use_default_colors ()) ;
	chk (cbreak ()) ;
	chk (noecho ()) ;
	chk (curs_set 0)

let exit () =
	endwin ()

let print x y c = if not (mvaddch y x (int_of_char c)) then errs := (Printf.sprintf "Cannot print at %d %d" x y)::!errs

let set_palette p f b = chk (init_pair p f b)

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
	val color = 0	(* By default, use default terminal color pair (white on black usually) *)
	(* val offset_x, offset_y, wrap_lines... *)
	method display x0 y0 width height =
		let put_chr (x, y) c =
			if y >= height then raise Exit ;
			if c = '\n' then (
				for x = x to width-1 (* width-2 fonctionne?!*) do print (x+x0) (y+y0) ' ' done ;
				0, y+1
			) else (
				if x < width then print (x+x0) (y+y0) c ;
				x+1, y
			) in
		attron (A.color_pair color) ;
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
	let root = Leaf (new text_buf (Rope.of_file "test.ml")) in
	let root = split ~split_dir:Vertical (new text_buf (Rope.of_string "blabla")) (Relative 0.5) root in
	display_root root ;
	chk (refresh ()) ;
	(* Then wait a key *)
	ignore (getch ())

let main =
	init () ;
	ignore_exceptions test () ;
	exit () ;
	List.iter (fun s -> Printf.printf "%s\n" s) !errs
