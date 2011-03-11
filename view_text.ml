open Otrui

module type VIEW_TEXT =
sig
	module Buf : BUF
	include VIEW_BASE

	val default_color             : Term.color_pair ref
	val default_no_content_color  : Term.color_pair ref
	val default_wrap_symbol_color : Term.color_pair ref
	val default_tab_width         : int ref

	val create : ?append:bool -> Buf.t -> t
end

module Make (Buf : BUF) (Term : TERM) (Cmd : CMD with module Term = Term) :
	VIEW_TEXT with module Buf = Buf and module Term = Term =
struct
	module Term = Term
	module Buf  = Buf
	module Rope = Buf.Rope

	type t =
		{ buf                       : Buf.t ;
		  mutable color             : Term.color_pair ;
		  mutable no_content_color  : Term.color_pair ;
		  mutable wrap_symbol_color : Term.color_pair ;
		  mutable cursor            : Buf.mark ;
		  mutable pos_first_line    : Buf.mark ;
		  mutable wrap_lines        : bool ;
		  mutable tab_width         : int ;
		  mutable offset_x          : int ;
		  mutable scroll_margin_y   : int ;
		  mutable scroll_margin_x   : int ;
		  mutable last_height       : int ;
		  mutable last_width        : int ;
		  key_bindings              : (key, t -> Buf.mark -> unit) Hashtbl.t }

	let text_views = ref []

	let default_color             = ref (0, false)
	let default_no_content_color  = ref (0, false)
	let default_wrap_symbol_color = ref (0, false)
	let default_tab_width         = ref 8

	let from_line_start t pos =
		let rec aux off pos =
			if pos = 0 || Rope.nth (Buf.content t.buf) (pos-1) = '\n' then off
			else aux (off+1) (pos-1) in
		aux 0 pos

	let to_line_end t pos =
		let rec aux off pos =
			if pos = Buf.length t.buf then off else
			if Rope.nth (Buf.content t.buf) pos = '\n' then off
			else aux (off+1) (pos+1) in
		aux 0 pos

	let rec nb_lines_between t pos1 pos2 =
		if pos2 >= pos1 then
			let s = Rope.sub (Buf.content t.buf) pos1 pos2 in
			Rope.fold_left (fun l c -> if c = '\n' then l+1 else l) 0 s
		else - (nb_lines_between t pos2 pos1)

	let more_lines_than n t pos =	(* Check that there are more than n lines from pos to end *)
		let s = Rope.sub (Buf.content t.buf) pos (Buf.length t.buf) in
		try ignore (Rope.fold_left (fun l c ->
			if c = '\n' then (if l >= n then raise Exit else l+1)
			else l) 1 s) ;
			false
		with Exit -> true

	exception Cannot_move

	let move_up t mark =
		let offset = from_line_start t (Buf.pos mark) in
		if offset = Buf.pos mark then raise Cannot_move ;
		let prev_line_len = from_line_start t ((Buf.pos mark) - offset - 1) in
		Buf.set_pos mark ((Buf.pos mark) - offset - 1 -
			(if prev_line_len >= offset then prev_line_len - offset else 0))

	let move_down t mark =
		let offset = from_line_start t (Buf.pos mark) in
		let to_end = to_line_end t (Buf.pos mark) in
		if (Buf.pos mark) + to_end >= Buf.length t.buf - 1 then raise Cannot_move ;
		let next_line_len = to_line_end t ((Buf.pos mark) + to_end + 1) in
		Buf.set_pos mark ((Buf.pos mark) + to_end + 1 +
			(if next_line_len >= offset then offset else next_line_len))

	let move_left _t mark =
		if Buf.pos mark <= 0 then raise Cannot_move ;
		Buf.set_pos mark ((Buf.pos mark) - 1)

	let move_right t mark =
		if Buf.pos mark >= Buf.length t.buf then raise Cannot_move ;
		Buf.set_pos mark ((Buf.pos mark) + 1)

	let move_page_up t mark = for i = 1 to t.last_height - 1 do move_up t mark done
	let move_page_down t mark = for i = 1 to t.last_height - 1 do move_down t mark done

	let move_to_line_start t mark =
		let disp = from_line_start t (Buf.pos mark) in
		Buf.set_pos mark ((Buf.pos mark) - disp)

	let move_to_line_end t mark =
		let disp = to_line_end t (Buf.pos mark) in
		Buf.set_pos mark ((Buf.pos mark) + disp)

	let move_to_start _t mark = Buf.set_pos mark 0
	let move_to_end t mark = Buf.set_pos mark (Buf.length t.buf)

	let insert_char_of_key k t mark =
		let c = Term.Key.to_char k in
		Buf.insert t.buf (Buf.pos mark) (Rope.singleton c)

	let backspace t mark =
		if Buf.pos mark == 0 then raise Cannot_move ;
		Buf.cut t.buf ((Buf.pos mark)-1) (Buf.pos mark)

	let delete t mark =
		if Buf.pos mark == Buf.length t.buf then raise Cannot_move ;
		Buf.cut t.buf (Buf.pos mark) ((Buf.pos mark) + 1)

	let delete_line t mark =
		let start = from_line_start t (Buf.pos mark)
		and stop = to_line_end t (Buf.pos mark) in
		Buf.cut t.buf start stop

	let set_default_key_bindings t =
		Hashtbl.clear t.key_bindings ;
		List.iter (fun (k, f) -> Hashtbl.add t.key_bindings k f)
			[ Term.Key.left,       move_left ;
			  Term.Key.right,      move_right ;
			  Term.Key.up,         move_up ;
			  Term.Key.down,       move_down ;
			  Term.Key.ppage,      move_page_up ;
			  Term.Key.npage,      move_page_down ;
			  Term.Key.of_int 1,   move_to_line_start ; (* Ctrl+A *)
			  Term.Key.of_int 5,   move_to_line_end ;	(* Ctrl+E *)
			  Term.Key.of_int 551, move_to_start ;	(* Ctrl+PPage *)
			  Term.Key.of_int 546, move_to_end ;	(* Ctrl+NPage *)
			  Term.Key.backspace,  backspace ;
			  Term.Key.delete,     delete ]

	let create ?(append=false) buf =
		let t =
			{ buf               = buf ;
			  color             = !default_color ;
			  no_content_color  = !default_no_content_color ;
			  wrap_symbol_color = !default_wrap_symbol_color ;
			  cursor            = Buf.mark buf (if append then Buf.length buf else 0) ;
			  pos_first_line    = Buf.mark buf 0 ;
			  wrap_lines        = false ;
			  tab_width         = !default_tab_width ;
			  offset_x          = 0 ;
			  scroll_margin_y   = 3 ;
			  scroll_margin_x   = 5 ;
			  last_height       = 1 ;
			  last_width        = 1 ;
			  key_bindings      = Hashtbl.create 11 } in
		set_default_key_bindings t ;
		text_views := t :: !text_views ;
		(* Remove our marks from buffer when we die (which won't happen until we get
		 * removed from !text_views *)
		Gc.finalise (fun t ->
			Buf.unmark t.buf t.cursor ;
			Buf.unmark t.buf t.pos_first_line) t ;
		t

	(* Reset starting position of display according to scroll_margin_y *)
	let center_cursor_y t height margin =
		assert (height >= 1) ;
		(* scroll up *)
		while
			let cursor_y = nb_lines_between t (Buf.pos t.pos_first_line) (Buf.pos t.cursor) in
			cursor_y < margin && Buf.pos t.pos_first_line > 0
		do
			move_up t t.pos_first_line
		done ;
		assert (Buf.pos t.pos_first_line <= Buf.pos t.cursor) ;
		(* scroll down *)
		while
			let cursor_y' = height - 1 - nb_lines_between t (Buf.pos t.pos_first_line) (Buf.pos t.cursor) in
			cursor_y' < margin && more_lines_than height t (Buf.pos t.pos_first_line)
		do
			move_down t t.pos_first_line
		done

	let content_descr t = Buf.name t.buf
	let content_status t =
		let len = Buf.length t.buf in
		Printf.sprintf "%d chars" len

	let draw t x0 y0 width height focused =
		Log.p "display %s from %d,%d, width=%d, height=%d" (Buf.name t.buf) x0 y0 width height ;
		t.last_height <- height ; t.last_width <- width ;
		center_cursor_y t height t.scroll_margin_y ;
		let rec put_chr (x, xl, y, n) c =
			(* x is the screen coordinate while xl is the number of advances (in screen positions)
			 * from the beginning of the line. This may become different after a line wrap *)
			if y >= height then raise Exit ;
			if c = '\n' then (
				let col = if focused && n = Buf.pos t.cursor then
					Term.reverse t.color else t.no_content_color in
				Term.set_color col ;
				for x = x to width-1 do
					Term.print (x+x0) (y+y0) (int_of_char ' ') ;
					Term.set_color t.no_content_color
				done ;
				0, t.offset_x, y+1, n+1
			) else (
				let char_repr xl = function
					| '\t' ->
						if t.tab_width < 1 then "" else
						let next_tab = ((xl / t.tab_width) + 1) * t.tab_width in
						String.make (next_tab - xl) ' '
					| c -> String.make 1 c in
				let s = char_repr xl c in
				let rec aux idx x xl y =
					if idx >= String.length s then x, xl, y else (
						if x < width-1 then (
							Term.set_color (
								if idx > 0 then t.no_content_color else
								if focused && n = Buf.pos t.cursor then Term.reverse t.color else t.color) ;
							Term.print (x+x0) (y+y0) (int_of_char s.[idx]) ;
							aux (idx+1) (x+1) (xl+1) y
						) else (
							Term.set_color t.wrap_symbol_color ;
							if t.wrap_lines then (
								Term.print (x+x0) (y+y0) (int_of_char '\\') ;
								aux idx 0 (xl+1) (y+1)
							) else (
								if x = width - 1 then	(* and not the last of this line *)
									Term.print (x+x0) (y+y0) (int_of_char '+') ;
								aux (idx+1) (x+1) (xl+1) y
							)
						)
					) in
				let next_x, next_xl, next_y = aux 0 x xl y in
				next_x, next_xl, next_y, n+1
			) in
		(* So that cursor is draw even when at the end of buffer *)
		let buf_eff = Rope.sub (Buf.content t.buf) (Buf.pos t.pos_first_line) (Buf.length t.buf) in
		let buf_eff = if focused && Buf.pos t.cursor = Buf.length t.buf then
			Rope.cat buf_eff (Rope.singleton ' ') else buf_eff in
		(try
			let x, _, y, _ = Rope.fold_left put_chr (0, t.offset_x, 0, Buf.pos t.pos_first_line) buf_eff in
			(* deletes the rest of the buffer *)
			let rec aux x y =
				if y < height then (
					if x < width then (
						Term.print (x+x0) (y+y0) (int_of_char ' ') ;
						aux (x+1) y
					) else aux 0 (y+1)
				) in
			Term.set_color t.no_content_color ;
			aux x y
		with Exit -> ())

	let key t k =
		Log.p "Got key %d" (Term.Key.to_int k) ;
		let f =
			try Hashtbl.find t.key_bindings k
			with Not_found -> insert_char_of_key k in
		(try f t t.cursor with Cannot_move -> Term.beep ())

	let execute t = function
		(* delete a whole line *)
		| [ d1 ; d2 ] when d1 = Cmd.c2i 'd' && d2 = Cmd.c2i 'd' ->
			(try delete_line t t.cursor
			with Not_found -> Cmd.error "Not in a text view")
		(* unknown command, try buf ? *)
		| x -> Buf.execute t.buf x

end

