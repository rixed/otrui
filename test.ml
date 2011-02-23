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
let print_string x y str =
	for i = 0 to (String.length str) - 1 do
		print (x+i) y str.[i]
	done

let set_palette p f b = chk (init_pair p f b)

let char_of_key k =
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

(* Buffers are somewhat editable content. *)

class virtual buffer =
object
	method virtual get : char Rope.t
	method virtual name : string
end

type text_buffer_mark = { mutable mark_pos: int }

class text_buffer init_content name =
object
	inherit buffer
	val mutable content = init_content
	method get = content
	method name = name

	val mutable marks = []
	(* Return a new mark at this position *)
	method mark pos =
		let mark = { mark_pos = pos } in
		marks <- mark :: marks ;
		mark
	
	method unmark mark = marks <- List.filter ((!=) mark) marks
		
	method insert pos c =
		let l = Rope.sub content 0 pos
		and r = Rope.sub content pos (Rope.length content) in
		content <- Rope.cat (Rope.cat l c) r ;
		let c_len = Rope.length c in
		let offset_mark_pos mark =
			if mark.mark_pos >= pos then mark.mark_pos <- mark.mark_pos + c_len in
		List.iter offset_mark_pos marks
	
	method delete pos n =
		let len = Rope.length content in
		let l = Rope.sub content 0 pos
		and r = if pos+n <= len-1 then Rope.sub content (pos+n) len else Rope.empty in
		Log.p "Deleting from '%s' gives '%s'+'%s'" (Rope.to_string content) (Rope.to_string l) (Rope.to_string r) ;
		content <- Rope.cat l r ;
		let update_mark_pos mark =
			if mark.mark_pos >= pos+n then mark.mark_pos <- mark.mark_pos - n
			else if mark.mark_pos >= pos then mark.mark_pos <- pos in
		List.iter update_mark_pos marks
end

(* There's only one repl buffer *)
let repl =
	let prompt = Rope.of_string "# " in
object
	inherit text_buffer prompt "REPL" as parent

	val mutable resp_end = { mark_pos = 0 }	(* not really *)
	initializer
		resp_end <- parent#mark (Rope.length prompt -1)

	method insert pos c =
		let top_eval cmd =
			let cmd = Rope.to_string cmd in
			Log.p "Executing '%s'" cmd ;
			let buffer = Buffer.create 100 in
			let fmt = Format.formatter_of_buffer buffer in
			let status =
				try
					let l = Lexing.from_string cmd in
					let ph = !Toploop.parse_toplevel_phrase l in
					Toploop.execute_phrase true fmt ph
				with exn ->
					Toploop.print_exception_outcome fmt exn ;
					false in
			status, Rope.of_string (Buffer.contents buffer) in
		let ends_with e r =
			let lr = Rope.length r in
			let le = String.length e in
			if lr >= le then (
				let e' = Rope.to_string (Rope.sub r (lr-le) lr) in
				e' = e
			) else false in
		let appending = pos = Rope.length content in
		parent#insert pos c ;
		if appending && ends_with ";;\n" content then (
			let cur_len = Rope.length content in
			let cmd = Rope.sub content (resp_end.mark_pos+1) cur_len in
			let _status, resp = top_eval cmd in
			parent#insert cur_len resp ;
			parent#insert (Rope.length content) prompt ;
			resp_end.mark_pos <- (Rope.length content) -1
		)
end

(* A view is a window that can be opened/closed/moved/resized
 * and which purpose is to display a buffer *)

type split_dir = Horizontal | Vertical 
type split_size = Absolute of int (* min abs size *) | Relative of float

let views = Hashtbl.create 20
let view_list () = Hashtbl.fold (fun n _ p -> n::p) views []
let get_view name = Hashtbl.find views name


class virtual view (name:string) =
object (self)
	initializer Hashtbl.add views name (self :> view)

	method virtual display : int -> int -> int -> int -> unit
	(* Et une autre pour interpreter une commande... c'est à dire qu'on peut appeler directement
	 * une autre méthode pour modifier l'état interne de buffer... *)
	method virtual key : int -> unit
	method virtual content_descr  : string (* left justified *)
	method virtual content_status : string (* right justified *)
end

class text_view name ?(append=false) buf =
	let from_line_start pos =
		let rec aux off pos =
			if pos = 0 || Rope.nth buf#get (pos-1) = '\n' then off
			else aux (off+1) (pos-1) in
		aux 0 pos
	and to_line_end pos =
		let rec aux off pos =
			if pos = Rope.length buf#get then off else
			if Rope.nth buf#get pos = '\n' then off
			else aux (off+1) (pos+1) in
		aux 0 pos in
	let nb_lines_between pos1 pos2 =
		let s = Rope.sub buf#get pos1 pos2 in
		Rope.fold_left (fun l c -> if c = '\n' then l+1 else l) 0 s in
	let more_lines_than n pos =	(* Check that there are more than n lines from pos to end *)
		let s = Rope.sub buf#get pos (Rope.length buf#get) in
		try ignore (Rope.fold_left (fun l c ->
			if c = '\n' then (if l >= n then raise Exit else l+1)
			else l) 1 s) ;
			false
		with Exit -> true
	in
object (self)
	inherit view name
	val mutable color = 0	(* By default, use default terminal color pair (white on black usually) *)
	val mutable no_content_color = 0
	val mutable wrap_symbol_color = 0
	val mutable wrap_lines = false

	method set_wrap ?(symbol_color) w =
		wrap_lines <- w ;
		wrap_symbol_color <- optdef symbol_color color

	val mutable pos_first_line = buf#mark 0 (* offset in buf of the first char of the first displayed line *)
	val mutable cursor = buf#mark (if append then Rope.length buf#get else 0)
	(* We start to scroll vertically if the cursor is less than this number of lines away
	 * from window border *)
	val mutable start_scroll_margin_y = 3
	val mutable start_scroll_margin_x = 5
	(* Reset starting position of display according to start_scroll_margin_y *)
	method center_cursor_y height margin =
		assert (height >= 1) ;
		(* scroll up *)
		while
			let cursor_y = nb_lines_between pos_first_line.mark_pos cursor.mark_pos in
			cursor_y < margin && pos_first_line.mark_pos > 0
		do
			pos_first_line.mark_pos <-
				let prev_line_len = from_line_start (pos_first_line.mark_pos-1) in
				pos_first_line.mark_pos - 1 - prev_line_len
		done ;
		assert (pos_first_line.mark_pos <= cursor.mark_pos) ;
		(* scroll down *)
		while
			let cursor_y' = height - 1 - nb_lines_between pos_first_line.mark_pos cursor.mark_pos in
			cursor_y' < margin && more_lines_than height pos_first_line.mark_pos
		do
			pos_first_line.mark_pos <-
				pos_first_line.mark_pos + to_line_end pos_first_line.mark_pos + 1
		done

	method content_descr = buf#name
	method content_status = "[--X---]"	(* TODO *)

	(* val offset_x, pos_first_line... *)
	method display x0 y0 width height =
		Log.p "display %s from %d,%d, width=%d, height=%d" buf#name x0 y0 width height ;
		self#center_cursor_y height start_scroll_margin_y ;
		let rec put_chr (x, y, n) c =
			if y >= height then raise Exit ;
			attrset (if n = cursor.mark_pos then A.reverse else A.normal) ;
			if c = '\n' then (
				for x = x to width-1 do
					print (x+x0) (y+y0) ' ' ;
					attrset A.normal
				done ;
				0, y+1, n+1
			) else (
				if wrap_lines then (
					if x < width-1 then (
						print (x+x0) (y+y0) c ;
						x+1, y, n+1
					) else (
						attrset (A.color_pair wrap_symbol_color) ;
						print (x+x0) (y+y0) '\\' ;
						attrset (A.color_pair color) ;
						put_chr (0, y+1, n) c
					)
				) else (
					if x < width-1 then (
						print (x+x0) (y+y0) c
					) else if x = width-1 (* and not the last or next newline *) then (
						attrset (A.color_pair wrap_symbol_color) ;
						print (x+x0) (y+y0) '+' ;
						attrset (A.color_pair color)
					) ;
					x+1, y, n+1
				)
			) in
		attrset (A.color_pair color) ;
		(* So that cursor is draw even when at the end of buffer *)
		let buf_eff = Rope.sub buf#get pos_first_line.mark_pos (Rope.length buf#get) in
		let buf_eff = Rope.cat buf_eff (Rope.singleton ' ') in
		(try
			let x, y, _ = Rope.fold_left put_chr (0, 0, pos_first_line.mark_pos) buf_eff in
			(* deletes the rest of the buffer *)
			let rec aux x y =
				if y < height then (
					if x < width then (
						print (x+x0) (y+y0) ' ' ;
						aux (x+1) y
					) else aux 0 (y+1)
				) in
			attrset (A.color_pair no_content_color) ;
			aux x y
		with Exit -> ()) ;

	method beep = ()

	method key k =
		Log.p "Got key %d" k ;
		(* Handle cursor movement *)
		if k = Key.left then (
			if cursor.mark_pos <= 0 then self#beep
			else cursor.mark_pos <- cursor.mark_pos - 1
		) else if k = Key.right then (
			if cursor.mark_pos >= Rope.length buf#get then self#beep
			else cursor.mark_pos <- cursor.mark_pos + 1
		) else if k = Key.up then (
			let offset = from_line_start cursor.mark_pos in
			if offset = cursor.mark_pos then self#beep else (
				let prev_line_len = from_line_start (cursor.mark_pos - offset - 1) in
				cursor.mark_pos <- cursor.mark_pos - offset - 1 -
					(if prev_line_len >= offset then prev_line_len - offset else 0)
			)
		) else if k = Key.down then (
			let offset = from_line_start cursor.mark_pos in
			let to_end = to_line_end cursor.mark_pos in
			if cursor.mark_pos + to_end >= Rope.length buf#get - 1 then self#beep else (
				let next_line_len = to_line_end (cursor.mark_pos + to_end + 1) in
				cursor.mark_pos <- cursor.mark_pos + to_end + 1 +
					(if next_line_len >= offset then offset else next_line_len)
			)
		(* Handle deletion *)
		) else if k = Key.backspace then (
			if cursor.mark_pos == 0 then self#beep
			else buf#delete (cursor.mark_pos-1) 1
		) else if k = Key.dc then (	(* delete *)
			if cursor.mark_pos == Rope.length buf#get then self#beep
			else buf#delete (cursor.mark_pos) 1
		) else (
			(* Other keys *)
			let c = char_of_key k in
			buf#insert cursor.mark_pos (Rope.singleton c)
		)

	(* Remove our marks from buffer when we die *)
	initializer Gc.finalise (fun _ -> buf#unmark cursor) self

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

let status_color = ref 3
let display_with_status view x0 y0 width height =
	let display_status view x0 y0 width =
		let descr  = view#content_descr
		and status = view#content_status in
		let descr_len  = String.length descr
		and status_len = String.length status in
		attrset (A.color_pair !status_color) ;
		for i = 0 to width - 1 do print (x0+i) y0 '-' done ;
		if width >= status_len then (
			print_string (x0+width-status_len) y0 status ;
			let width = width - status_len - 1 in
			if width >= descr_len then
				print_string x0 y0 descr
			else
				print_string x0 y0 (String.sub descr 0 width)
		) in
	assert (height >= 1) ;
	if height > 1 then view#display x0 y0 width (height-1) ;
	display_status view x0 (y0+height-1) width

let show_vert_split = ref true
let vert_split_color = ref !status_color
let rec display x0 y0 width height = function
	| Leaf view -> display_with_status view x0 y0 width height
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
				if width > 0 && height > 0 then (
					if dir = Vertical && !show_vert_split then (
						if width > 1 then display x0 y0 (width-1) height win ;
						attrset (A.color_pair !vert_split_color) ;
						for y = 0 to (height-1) do
							print (x0+width-1) (y0+y) '|'
						done
					) else (
						display x0 y0 width height win
					)
				) ;
				let next_c = match dir with Horizontal -> y0 + height | Vertical -> x0 + width in
				aux next_c children' in
		aux 0 children

let root =
	let content = new text_buffer (Rope.of_file "test.ml") "test.ml" in
	let v1 = new text_view "view1" content
	and v2 = new text_view "view2" content in
	v1#set_wrap ~symbol_color:1 true ;
	v2#set_wrap ~symbol_color:1 false ;
	let top = split ~split_dir:Vertical (v2:>view) (Relative 0.5) (Leaf (v1:>view)) in
	let repl_view = new text_view "repl" ~append:true repl in
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

let key_loop () =
	Log.p "Loop for getch" ;
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
			next_key ()
		) in
	next_key ()

(* works only if a function... If not, cannot access to these symbols :-/ *)
let start () =
	init_ncurses () ;
	key_loop ()

