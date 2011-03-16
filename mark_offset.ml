open Otrui

(* Simple implementation of marks with only an offset in the buffer *)

module Make =
struct
	type t = { mutable pos: int }

	let create () = { pos = 0 }

	let to_beginning_of m _ = m.pos <- 0
	let to_end_of m r = m.pos <- Rope.length r
	let update_for_insert m _ pos r' =
		if m.pos >= pos then m.pos <- m.pos + Rope.length r'
	let update_for_cut m _ start stop =
		if m.pos >= stop then m.pos <- m.pos - (stop-start)
		else if m.pos >= start then m.pos <- start
	let pos m = m.pos
end

