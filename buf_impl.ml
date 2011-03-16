open Otrui

module type S =
sig
	include BUF
	val create : unit -> t
end

module Make (Cmd : CMD) : S =
struct

	type t =
		{ mutable content : char Rope.t ;
		  mutable marks   : mark list }

	let create () =
		{ content = Rope.empty ;
		  marks = [] }

	let content t = t.content

	let mark t m = 
		t.marks <- m :: t.marks

	let unmark t m =
		t.marks <- List.filter ((!=) m) t.marks

	let insert t pos c =
		t.content <- Rope.insert t.content pos c ;
		List.iter (fun m -> m.update_for_insert t.content pos c) t.marks
	
	let cut t start stop =
		assert (stop >= start) ;
		List.iter (fun m -> m.update_for_cut t.content start stop) t.marks ;
		Log.p "Cutting '%s' from %d to %d..." (Rope.to_string t.content) start stop ;
		t.content <- Rope.cut t.content start stop ;
		Log.p "Got : '%s'" (Rope.to_string t.content)

	let append t c = insert t (Rope.length (content t)) c
	let append_string t s = insert t (Rope.length (content t)) (Rope.of_string s)
	let length t = Rope.length (content t)

	let execute _ _ = raise Cmd.Unknown
end
