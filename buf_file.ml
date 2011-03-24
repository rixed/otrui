open Bricabrac
open Otrui

module Make (Buf : Buf_impl.S) =
struct
	module Buf = Buf

	type t =
		{ buf           : Buf.t ;
		  fname         : string ;
		  mutable clean : bool ;
		  mutable saved : float option }

	let create fname =
		let buf = Buf.create () in
		Buf.append buf (Rope.make_unknown (Rope.of_file fname)) ;
		Buf.reset_undo buf ;
		{ buf = buf ;
		  fname = fname ;
		  clean = true ;
		  saved = None }

	let trashed t f = t.clean <- false ; f t.buf

	let content t = Buf.content t.buf
	let mark t    = Buf.mark t.buf
	let unmark t  = Buf.unmark t.buf
	let length t  = Buf.length t.buf
	let append t  = trashed t Buf.append
	let undo t    = trashed t Buf.undo
	let redo t    = trashed t Buf.redo
	let insert t  = trashed t Buf.insert
	let cut t     = trashed t Buf.cut
end
