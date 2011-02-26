(* Buffers are somewhat editable content. *)
module Rope = Rope_impl.Make

class virtual t =
object
	method virtual get : char Rope.t
	method virtual name : string
end

type mark = { mutable pos: int }

class text init_content name =
object
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
	
	method delete start stop =
		assert (stop >= start) ;
		content <- Rope.cut content start stop ;
		let update_mark mark =
			if mark.pos >= stop then mark.pos <- mark.pos - (stop-start)
			else if mark.pos >= start then mark.pos <- start in
		List.iter update_mark marks
end

(* There's only one repl buffer *)
let repl =
	let prompt = Rope.of_string "# " in
object (self)
	inherit text prompt "REPL" as parent

	val mutable resp_end = { pos = 0 }	(* not really *)
	initializer
		resp_end <- parent#mark (Rope.length prompt -1)

	method insert pos c =
		let top_eval cmd =
			let cmd = Rope.to_string cmd in
			Log.p "Executing '%s'" cmd ;
			let buffer = Buffer.create 100 in
			let fmt = Format.formatter_of_buffer buffer in
			let status =
				try
					let l = Lexing.from_string cmd in
					let ph = !Toploop.parse_toplevel_phrase l in
					Toploop.execute_phrase true fmt ph
				with exn ->
					Toploop.print_exception_outcome fmt exn ;
					false in
			status, Rope.of_string (Buffer.contents buffer) in
		let ends_with e r =
			let lr = Rope.length r in
			let le = String.length e in
			if lr >= le then (
				let e' = Rope.to_string (Rope.sub r (lr-le) lr) in
				e' = e
			) else false in
		let appending = pos = Rope.length content in
		parent#insert pos c ;
		if appending && ends_with ";;\n" content then (
			let cur_len = Rope.length content in
			assert (resp_end.pos < cur_len) ;
			let cmd = Rope.sub content (resp_end.pos+1) cur_len in
			let _status, resp = top_eval cmd in
			parent#insert cur_len resp ;
			parent#insert (Rope.length content) prompt ;
			resp_end.pos <- (Rope.length content) -1
		)

	(* shortcut *)
	method append c = self#insert (Rope.length content) c

	(* Disallow to delete the last prompt *)
	method delete start stop =
		let prompt_stop = resp_end.pos + 1 in
		let prompt_start = prompt_stop - (Rope.length prompt) in
		if start < prompt_stop && stop > prompt_start then (
			(* do nothing *)
		) else (
			content <- Rope.cut content start stop ;
			let update_mark mark =
				if mark.pos >= stop then mark.pos <- mark.pos - (stop-start)
				else if mark.pos >= start then mark.pos <- start in
			List.iter update_mark marks
		)

end


