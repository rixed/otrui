let doc = "Buffers are somewhat editable content."

open Bricabrac
module Rope = Rope_impl.Make

class virtual t =
object
	method virtual get : char Rope.t
	method virtual name : string
end

type mark = { mutable pos: int }

class text init_content name =
object (self)
	inherit t
	val mutable content = init_content
	method get = content
	method name = name

	val mutable marks = []
	(* Return a new mark at this position *)
	method mark pos =
		let mark = { pos = pos } in
		marks <- mark :: marks ;
		mark
	
	method unmark mark = marks <- List.filter ((!=) mark) marks
		
	method insert pos c =
		content <- Rope.insert content pos c ;
		let c_len = Rope.length c in
		let offset_mark mark =
			if mark.pos >= pos then mark.pos <- mark.pos + c_len in
		List.iter offset_mark marks
	
	(* shortcut *)
	method append c = self#insert (Rope.length content) c

	method delete start stop =
		assert (stop >= start) ;
		content <- Rope.cut content start stop ;
		let update_mark mark =
			if mark.pos >= stop then mark.pos <- mark.pos - (stop-start)
			else if mark.pos >= start then mark.pos <- start in
		List.iter update_mark marks
end

