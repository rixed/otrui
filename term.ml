module Key = Curses.Key
open Curses

let chk ok =
	if not ok then (
		Log.p "ncurses error!" ;
		failwith "Error"
	)

let print x y c = if not (mvaddch y x c) then Log.p "Cannot print at %d %d" x y
let print_string x y str =
	for i = 0 to (String.length str) - 1 do
		print (x+i) y (int_of_char str.[i])
	done

let next_avl_pair = ref 1 (* we do not redefine 0 that we keep for when we lack pairs *)
let known_pairs : ((((int*int*int)*(int*int*int)),int) Hashtbl.t) = Hashtbl.create 100 (* from (fg r,g,b), (bg r,g,b) to color_pair number *)
let next_avl_color = ref 0
let known_colors = Hashtbl.create 100 (* from (r,g,b) to color number *)

let find_color (r, g, b) =
	try Hashtbl.find known_colors (r, g, b)
	with Not_found ->
		if can_change_color () && !next_avl_color < colors () then (
			let c = !next_avl_color in
			chk (init_color c r g b) ;
			Hashtbl.add known_colors (r, g, b) c ;
			incr next_avl_color ;
			c
		) else (
			(* look for something close *)
			let dist_color (r1, g1, b1) (r2, g2, b2) = abs (r1-r2) + abs (g1-g2) + abs (b1-b2) in
			let choose_min rgb c (dist_min, c_min) =
				let dist = dist_color rgb (r, g, b) in
				if dist < dist_min then dist, c else dist_min, c_min in
			let _, best_c = Hashtbl.fold choose_min known_colors (max_int, 0) in
			best_c
		)

let build_pair (fg:int*int*int) (bg:int*int*int) =
	let p = !next_avl_pair in
	if p < color_pairs () then (
		let fg_c = find_color fg and bg_c = find_color bg in
		chk (init_pair p fg_c bg_c) ;
		Hashtbl.add known_pairs (fg, bg) p ;
		incr next_avl_pair ;
		p
	) else 0 (* ? *)

let set_color ((fg:int*int*int), (bg:int*int*int)) =
	let pair, need_reverse =
		try Hashtbl.find known_pairs (fg, bg), false
		with Not_found ->
			try Hashtbl.find known_pairs (bg, fg), true
			with Not_found -> build_pair fg bg, false
	in
	attrset (A.color_pair pair) ;
	if need_reverse then attrset A.reverse

let reverse (fg, bg) = bg, fg

let palette_reset () =
	next_avl_pair := 1 ;
	next_avl_color := 0 ;
	Hashtbl.clear known_pairs ;
	Hashtbl.clear known_colors

let redisplay () =
	chk (refresh ())
(*	palette_reset ()*)
	
let ascii_escape = 27
let ascii_return = 13

let key () = getch ()

let char_of_key k =
	if k = Key.eol || k = ascii_return then '\n'
	(* TODO: handle DEL, backspace... *)
	(*else if k > 255 then '?'*)
	else char_of_int k

let screen_size () =
	let win = stdscr () in
	let height, width = getmaxyx win in
	width, height

let init () =
	Log.p "Initializing ncurses..." ;
	ignore (initscr ()) ;
	assert (has_colors ()) ; (* FIXME *)
	Log.p "  Starting colors..." ;
	chk (start_color ()) ;
	Log.p "  cbreak..." ;
	chk (cbreak ()) ;
	Log.p "  noecho..." ;
	chk (noecho ()) ;
	Log.p "  nonl..." ;
	nonl () ;
	Log.p "  keypad..." ;
	chk (keypad (stdscr ()) true) ;
	Log.p "  hide cursor..." ;
	chk (curs_set 0) ;
	let height, width = getmaxyx (stdscr ()) in
	Log.p "  resolution = %d x %d" width height ;
	Log.p "  max colors = %d" (colors ()) ;
	Log.p "  max color pairs = %d" (color_pairs ())

let quit () =
	Log.p "Exiting ncurses" ;
	endwin ()

let hline, vline = int_of_char '-', int_of_char '|'
(*	let acs = get_acs_codes () in
	acs.Acs.hline, acs.Acs.vline
*)

let is_direction k = k = Key.up || k = Key.down || k = Key.left || k = Key.right

