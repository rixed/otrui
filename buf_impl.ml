open Otrui

module Make (Buf : BUF_BASE) :
	BUF with type t = Buf.t =
struct
	include Buf

	let append t c = insert t (Rope.length (content t)) c

	let append_string t s = insert t (Rope.length (content t)) (Rope.of_string s)

	let length t = Rope.length (content t)
end
