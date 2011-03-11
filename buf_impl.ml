open Otrui

module type S =
sig
	include BUF
	val create : unit -> t
end

module Make (Cmd : CMD) : S =
struct
	type mark = { mutable pos: int }
	type t =
		{ mutable content : char Rope.t ;
		  mutable marks   : mark list }

	let create () =
		{ content = Rope.empty ;
		  marks = [] }

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

	let append t c = insert t (Rope.length (content t)) c
	let append_string t s = insert t (Rope.length (content t)) (Rope.of_string s)
	let length t = Rope.length (content t)

	let execute _ _ = raise Cmd.Unknown
end
