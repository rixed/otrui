module Color = Curses.Color
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
let palette = Hashtbl.create 100
let palette_reset () =
	next_avl_pair := 1 ;
	Hashtbl.clear palette
let set_palette p fg bg =
	let c = colors () in
	chk (init_pair p (fg mod c) (bg mod c))

let set_color (fg, bg) =
	let pair, need_reverse =
		try Hashtbl.find palette (fg, bg), false
		with Not_found ->
			try Hashtbl.find palette (bg, fg), true
			with Not_found ->
				let p = !next_avl_pair in
				if p < color_pairs () then (
					set_palette p fg bg ;
					Hashtbl.add palette (fg, bg) p ;
					incr next_avl_pair ;
					p, false
				) else (
					0, false
				)
	in
	attrset (A.color_pair pair) ;
	if need_reverse then attrset A.reverse

let reverse (fg, bg) = bg, fg
let redisplay () =
	chk (refresh ()) ;
	palette_reset ()
	
let key () = getch ()

let char_of_key k =
	if k = Key.eol || k = 13 then '\n'
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
