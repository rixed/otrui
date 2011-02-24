(* A view is a window that can be opened/closed/moved/resized
 * and which purpose is to display a buffer *)
open Bricabrac
module Rope = Buf.Rope

let views = Hashtbl.create 20
let view_list () = Hashtbl.fold (fun n _ p -> n::p) views []
let get_view name = Hashtbl.find views name
let default_color = Term.Color.white, Term.Color.black

class virtual t (name:string) =
object (self)
	initializer Hashtbl.add views name (self :> t)

	method virtual display : int -> int -> int -> int -> unit
	(* Et N autres pour interpreter une commande... c'est à dire qu'on peut appeler directement
	 * une autre méthode pour modifier l'état interne de buffer... *)
	method virtual key : int -> unit
	method virtual content_descr  : string (* left justified *)
	method virtual content_status : string (* right justified *)
end

class text name ?(append=false) buf =
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
	inherit t name
	val mutable color = default_color
	val mutable no_content_color = default_color
	val mutable wrap_symbol_color = default_color
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
			let cursor_y = nb_lines_between pos_first_line.Buf.pos cursor.Buf.pos in
			cursor_y < margin && pos_first_line.Buf.pos > 0
		do
			pos_first_line.Buf.pos <-
				let prev_line_len = from_line_start (pos_first_line.Buf.pos-1) in
				pos_first_line.Buf.pos - 1 - prev_line_len
		done ;
		assert (pos_first_line.Buf.pos <= cursor.Buf.pos) ;
		(* scroll down *)
		while
			let cursor_y' = height - 1 - nb_lines_between pos_first_line.Buf.pos cursor.Buf.pos in
			cursor_y' < margin && more_lines_than height pos_first_line.Buf.pos
		do
			pos_first_line.Buf.pos <-
				pos_first_line.Buf.pos + to_line_end pos_first_line.Buf.pos + 1
		done

	method content_descr = buf#name
	method content_status = "[--X---]"	(* TODO *)

	(* val offset_x, pos_first_line... *)
	method display x0 y0 width height =
		Log.p "display %s from %d,%d, width=%d, height=%d" buf#name x0 y0 width height ;
		self#center_cursor_y height start_scroll_margin_y ;
		let rec put_chr (x, y, n) c =
			if y >= height then raise Exit ;
			Term.set_color (if n = cursor.Buf.pos then Term.reverse color else color) ;
			if c = '\n' then (
				for x = x to width-1 do
					Term.print (x+x0) (y+y0) (int_of_char ' ') ;
					Term.set_color color
				done ;
				0, y+1, n+1
			) else (
				if wrap_lines then (
					if x < width-1 then (
						Term.print (x+x0) (y+y0) (int_of_char c) ;
						x+1, y, n+1
					) else (
						Term.set_color wrap_symbol_color ;
						Term.print (x+x0) (y+y0) (int_of_char '\\') ;
						Term.set_color color ;
						put_chr (0, y+1, n) c
					)
				) else (
					if x < width-1 then (
						Term.print (x+x0) (y+y0) (int_of_char c)
					) else if x = width-1 (* and not the last or next newline *) then (
						Term.set_color wrap_symbol_color ;
						Term.print (x+x0) (y+y0) (int_of_char '+') ;
						Term.set_color color
					) ;
					x+1, y, n+1
				)
			) in
		Term.set_color color ;
		(* So that cursor is draw even when at the end of buffer *)
		let buf_eff = Rope.sub buf#get pos_first_line.Buf.pos (Rope.length buf#get) in
		let buf_eff = Rope.cat buf_eff (Rope.singleton ' ') in
		(try
			let x, y, _ = Rope.fold_left put_chr (0, 0, pos_first_line.Buf.pos) buf_eff in
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
		with Exit -> ()) ;

	method beep = ()

	method key k =
		Log.p "Got key %d" k ;
		(* Handle cursor movement *)
		if k = Term.Key.left then (
			if cursor.Buf.pos <= 0 then self#beep
			else cursor.Buf.pos <- cursor.Buf.pos - 1
		) else if k = Term.Key.right then (
			if cursor.Buf.pos >= Rope.length buf#get then self#beep
			else cursor.Buf.pos <- cursor.Buf.pos + 1
		) else if k = Term.Key.up then (
			let offset = from_line_start cursor.Buf.pos in
			if offset = cursor.Buf.pos then self#beep else (
				let prev_line_len = from_line_start (cursor.Buf.pos - offset - 1) in
				cursor.Buf.pos <- cursor.Buf.pos - offset - 1 -
					(if prev_line_len >= offset then prev_line_len - offset else 0)
			)
		) else if k = Term.Key.down then (
			let offset = from_line_start cursor.Buf.pos in
			let to_end = to_line_end cursor.Buf.pos in
			if cursor.Buf.pos + to_end >= Rope.length buf#get - 1 then self#beep else (
				let next_line_len = to_line_end (cursor.Buf.pos + to_end + 1) in
				cursor.Buf.pos <- cursor.Buf.pos + to_end + 1 +
					(if next_line_len >= offset then offset else next_line_len)
			)
		(* Handle deletion *)
		) else if k = Term.Key.backspace then (
			if cursor.Buf.pos == 0 then self#beep
			else buf#delete (cursor.Buf.pos-1) 1
		) else if k = Term.Key.dc then (	(* delete *)
			if cursor.Buf.pos == Rope.length buf#get then self#beep
			else buf#delete (cursor.Buf.pos) 1
		) else (
			(* Other keys *)
			let c = Term.char_of_key k in
			buf#insert cursor.Buf.pos (Rope.singleton c)
		)

	(* Remove our marks from buffer when we die *)
	initializer Gc.finalise (fun _ -> buf#unmark cursor) self

end


