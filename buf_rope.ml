open Otrui

module Make (Cmd : CMD) : BUF_BASE =
struct
	module Rope = Rope_impl.Make

	type mark = { mutable pos: int }
	type t =
		{ mutable content : char Rope.t ;
		  mutable marks   : mark list ;
		  name            : string }

	let create name content =
		{ content = content ;
		  marks = [] ;
		  name = name }

	let name t = t.name

	let content t = t.content

	let mark t pos = 
		let m = { pos = pos } in
		t.marks <- m :: t.marks ;
		m

	let unmark t mark =
		t.marks <- List.filter ((!=) mark) t.marks

	let pos mark = mark.pos

	let set_pos mark pos = mark.pos <- pos

	let insert t pos c =
		t.content <- Rope.insert t.content pos c ;
		let c_len = Rope.length c in
		let offset_mark mark =
			if mark.pos >= pos then mark.pos <- mark.pos + c_len in
		List.iter offset_mark t.marks

	let cut t start stop =
		assert (stop >= start) ;
		t.content <- Rope.cut t.content start stop ;
		let update_mark mark =
			if mark.pos >= stop then mark.pos <- mark.pos - (stop-start)
			else if mark.pos >= start then mark.pos <- start in
		List.iter update_mark t.marks

	let execute _ _ = raise Cmd.Unknown
end
