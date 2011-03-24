open Otrui

exception Cannot_move

module MarkLine = Mark_lines.Make
module Mark = Mark_impl.Make (MarkLine)

module Make (Buf : BUF) (Term : TERM) (Cmd : CMD with module Term = Term) =
struct
	module Term = Term
	module Buf  = Buf

	type t =
		{ buf                       : Buf.t ;
		  colors                    : (Rope.anot, Term.color_pair) Hashtbl.t ;
		  mutable default_color     : Term.color_pair ;
		  mutable cursor            : MarkLine.t ;
		  mutable pos_first_line    : MarkLine.t ;
		  mutable wrap_lines        : bool ;
		  mutable tab_width         : int ;
		  mutable offset_x          : int ;
		  mutable scroll_margin_y   : int ;
		  mutable scroll_margin_x   : int ;
		  mutable last_height       : int ;
		  mutable last_width        : int ;
		  key_bindings              : (key, t -> MarkLine.t -> unit) Hashtbl.t }

	let text_views = ref []
	let current = ref None

	let default_tab_width  = ref 8
	let default_color      = ref (Term.get_color (1000, 1000, 1000) (0, 0, 0))

	(* Helper functions *)

	let nb_lines_between m1 m2 = m2.MarkLine.lineno - m1.MarkLine.lineno

	let move_to_line_start _ m = m.MarkLine.lineoff <- 0

	let line_stop t m =
		let rec aux c =
			if c < Buf.length t.buf && fst (Rope.nth (Buf.content t.buf) c) <> '\n' then aux (c+1)
			else c in
		aux (MarkLine.pos m)

	let line_start _ m = m.MarkLine.linepos

	let rec move_to_line_end t m =
		m.MarkLine.lineoff <- line_stop t m - m.MarkLine.linepos

	let move_right t m =
		let p = MarkLine.pos m in
		if Buf.length t.buf <= p then raise Cannot_move ;
		if fst (Rope.nth (Buf.content t.buf) p) = '\n' then (
			m.MarkLine.linepos <- m.MarkLine.linepos + m.MarkLine.lineoff + 1 ;
			m.MarkLine.lineoff <- 0 ;
			m.MarkLine.lineno <- m.MarkLine.lineno + 1
		) else
			m.MarkLine.lineoff <- m.MarkLine.lineoff + 1

	let move_left t m =
		if m.MarkLine.lineoff > 0 then
			m.MarkLine.lineoff <- m.MarkLine.lineoff - 1
		else if m.MarkLine.lineno > 0 then (
			m.MarkLine.lineno <- m.MarkLine.lineno - 1 ;
			m.MarkLine.lineoff <- 0 ;
			assert (m.MarkLine.linepos > 0) ;
			m.MarkLine.linepos <- m.MarkLine.linepos - 1 ;
			assert (fst (Rope.nth (Buf.content t.buf) m.MarkLine.linepos) = '\n') ;
			while
				m.MarkLine.linepos > 0 &&
				fst (Rope.nth (Buf.content t.buf) (m.MarkLine.linepos - 1)) <> '\n'
			do
				m.MarkLine.linepos <- m.MarkLine.linepos - 1 ;
				m.MarkLine.lineoff <- m.MarkLine.lineoff + 1
			done
		) else raise Cannot_move

	let move_up t m =
		if m.MarkLine.linepos = 0 then raise Cannot_move ;
		let off = m.MarkLine.lineoff in
		move_to_line_start t m ;
		move_left t m ;
		if m.MarkLine.lineoff > off then m.MarkLine.lineoff <- off

	let move_down t m =
		let off = m.MarkLine.lineoff in
		move_to_line_end t m ;
		move_right t m ;
		move_to_line_end t m ;
		if m.MarkLine.lineoff > off then m.MarkLine.lineoff <- off

	let more_lines_than h t m =
		let m' = { m with MarkLine.linepos = m.MarkLine.linepos } in
		let rec aux h =
			if h = 0 then true else (
				move_down t m' ;
				aux (h-1)
			) in
		try aux h with Cannot_move -> false
		
	let move_page_up t mark = for i = 1 to t.last_height - 1 do move_up t mark done
	let move_page_down t mark = for i = 1 to t.last_height - 1 do move_down t mark done

	let insert_char_of_key k t mark =
		let c = Term.Key.to_char k in
		Buf.insert t.buf (MarkLine.pos mark) (Rope.singleton (c, Rope.none))

	let backspace t mark =
		if MarkLine.pos mark == 0 then raise Cannot_move ;
		Buf.cut t.buf ((MarkLine.pos mark)-1) (MarkLine.pos mark)

	let delete t mark =
		if MarkLine.pos mark == Buf.length t.buf then raise Cannot_move ;
		Buf.cut t.buf (MarkLine.pos mark) ((MarkLine.pos mark) + 1)

	let delete_line t mark =
		Log.p "deleting line..." ;
		let start = line_start t mark
		and stop = line_stop t mark in
		(* If we stopped on the '\n', skip over it *)
		let stop = if stop < Buf.length t.buf then stop+1 else stop in
		Buf.cut t.buf start stop

	let set_default_key_bindings t =
		let to_mark f t m = f m (Buf.content t.buf) in
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
			  Term.Key.home,       to_mark MarkLine.to_beginning_of ;
			  Term.Key.end_,       to_mark MarkLine.to_end_of ;
			  Term.Key.of_int 551, to_mark MarkLine.to_beginning_of ;	(* Ctrl+PPage *)
			  Term.Key.of_int 546, to_mark MarkLine.to_end_of ;	(* Ctrl+NPage *)
			  Term.Key.backspace,  backspace ;
			  Term.Key.delete,     delete ]

	let create ?(append=false) buf =
		let t =
			{ buf               = buf ;
			  colors            = Hashtbl.create 11 ;
			  default_color     = !default_color ;
			  cursor            = MarkLine.create () ;
			  pos_first_line    = MarkLine.create () ;
			  wrap_lines        = false ;
			  tab_width         = !default_tab_width ;
			  offset_x          = 0 ;
			  scroll_margin_y   = 3 ;
			  scroll_margin_x   = 5 ;
			  last_height       = 1 ;
			  last_width        = 1 ;
			  key_bindings      = Hashtbl.create 11 } in
		if append then MarkLine.to_end_of t.cursor (Buf.content buf) ;
		let cursor = Mark.mark t.cursor and pos_first_line = Mark.mark t.pos_first_line in
		Buf.mark t.buf cursor ;
		Buf.mark t.buf pos_first_line ;
		set_default_key_bindings t ;
		text_views := t :: !text_views ;
		(* Remove our marks from buffer when we die (which won't happen until we get
		 * removed from !text_views *)
		Gc.finalise (fun t ->
			Buf.unmark t.buf cursor ;
			Buf.unmark t.buf pos_first_line) t ;
		t

	(* Reset starting position of display according to scroll_margin_y *)
	let center_cursor_y t height margin =
		assert (height >= 1) ;
		(* scroll up *)
		while
			let cursor_y = nb_lines_between t.pos_first_line t.cursor in
			cursor_y < margin && MarkLine.pos t.pos_first_line > 0
		do
			move_up t t.pos_first_line
		done ;
		assert (MarkLine.pos t.pos_first_line <= MarkLine.pos t.cursor) ;
		(* scroll down *)
		while
			let cursor_y' = height - 1 - nb_lines_between t.pos_first_line t.cursor in
			cursor_y' < margin && more_lines_than height t t.pos_first_line
		do
			move_down t t.pos_first_line
		done

	let content_status t =
		let len = Buf.length t.buf in
		Printf.sprintf "l:%d,%d c:%d/%d (curs:%d,%d,%d) %s"
			t.cursor.MarkLine.lineno t.cursor.MarkLine.lineoff (MarkLine.pos t.cursor) len
			t.cursor.MarkLine.linepos t.cursor.MarkLine.lineoff t.cursor.MarkLine.lineno
			(Buf.status t.buf)

	let color_of_achar t _x _xl _y (_, anot) =
		try Hashtbl.find t.colors anot with Not_found -> t.default_color
	
	let no_content = Rope.anot_of_string "no content"
	let wrap_mark = Rope.anot_of_string "wrap mark"

	let draw t x0 y0 width height focused =
		Log.p "display from %d,%d, width=%d, height=%d" x0 y0 width height ;
		t.last_height <- height ; t.last_width <- width ;
		current := if focused then Some t else None ;
		center_cursor_y t height t.scroll_margin_y ;
		let no_content_color = color_of_achar t 0 0 0 (' ', no_content) in
		let wrap_mark_color = color_of_achar t 0 0 0 (' ', wrap_mark) in
		let rec put_chr (x, xl, y, n) c =
			(* x is the screen coordinate while xl is the number of advances (in screen positions)
			 * from the beginning of the line. This may become different after a line wrap *)
			if y >= height then raise Exit ;
			if fst c = '\n' then (
				let col =
					if focused && n = MarkLine.pos t.cursor then
						Term.reverse (color_of_achar t x xl y (' ', Rope.none))
					else no_content_color in
				Term.set_color col ;
				for x = x to width-1 do
					Term.print (x+x0) (y+y0) (int_of_char ' ') ;
					Term.set_color no_content_color
				done ;
				0, t.offset_x, y+1, n+1
			) else (
				let char_repr xl = function
					| '\t', _ ->
						if t.tab_width < 1 then "" else
						let next_tab = ((xl / t.tab_width) + 1) * t.tab_width in
						String.make (next_tab - xl) ' '
					| c, _ -> String.make 1 c in
				let s = char_repr xl c in
				let rec aux idx x xl y =
					if idx >= String.length s then x, xl, y else (
						if x < width-1 then (
							let color = if idx > 0 then no_content_color else
								let col = color_of_achar t x xl y c in
								if focused && n = MarkLine.pos t.cursor then Term.reverse col
								else col in
							Term.set_color color ;
							Term.print (x+x0) (y+y0) (int_of_char s.[idx]) ;
							aux (idx+1) (x+1) (xl+1) y
						) else (
							Term.set_color wrap_mark_color ;
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
		let buf_eff = Rope.sub (Buf.content t.buf) (MarkLine.pos t.pos_first_line) (Buf.length t.buf) in
		let buf_eff = if focused && MarkLine.pos t.cursor = Buf.length t.buf then
			Rope.cat buf_eff (Rope.singleton (' ', Rope.none)) else buf_eff in
		(try
			let x, _, y, _ = Rope.fold_left put_chr (0, t.offset_x, 0, MarkLine.pos t.pos_first_line) buf_eff in
			(* deletes the rest of the buffer *)
			let rec aux x y =
				if y < height then (
					if x < width then (
						Term.print (x+x0) (y+y0) (int_of_char ' ') ;
						aux (x+1) y
					) else aux 0 (y+1)
				) in
			Term.set_color no_content_color ;
			aux x y
		with Exit -> ())

	let key t k =
		Log.p "Got key %d" (Term.Key.to_int k) ;
		let f =
			try Hashtbl.find t.key_bindings k
			with Not_found -> insert_char_of_key k in
		(try f t t.cursor with Cannot_move -> Term.beep ())
end


