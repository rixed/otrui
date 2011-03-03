(* This is a plugin for otrui that show how to implement a new kind of buffer.
 * This one implements a pipe, ie. everything you type in this buffer is send as stdin
 * to a program, and every output of the program is appended to the buffer. *)
open Bricabrac
open Buf

let doc = "This modules implements a buffer type suitable for controlling an external program"

let _ = repl#append (Rope.of_string "Loading pipe buffer\n")

class t program =
	let prompt = Rope.of_string ">% "
	and ch_in, ch_out = Unix.open_process program in
object (self)
	inherit text prompt (Printf.sprintf "|%s" program) as parent

	val mutable resp_end = { pos = 0 }
	val mutable reader_thread = None

	method read_output () =
		let aux () =
			let line = input_line ch_in in
			Log.p "Receiving string '%s' from program '%s'" line program ;
			let line = Rope.cat (Rope.of_string line) (Rope.singleton '\n') in
			let pos = resp_end.pos + 1 - (Rope.length prompt) in
			parent#insert pos line in
		forever aux ()

	initializer
		resp_end <- parent#mark (Rope.length prompt -1) ;
		Gc.finalise (fun _ -> ignore (Unix.close_process (ch_in, ch_out))) self ;
		reader_thread <- Some (Thread.create self#read_output ())

	method append_prompt =
		if resp_end.pos < (Rope.length content) -1 then (
			parent#append prompt ;
			resp_end.pos <- (Rope.length content) -1
		)

	method insert pos c =
		let appending = pos = Rope.length content in
		parent#insert pos c ;
		if appending &&
		   Rope.length content > 0 &&
		   Rope.nth content ((Rope.length content) -1) = '\n' then (
			let cur_len = Rope.length content in
			assert (resp_end.pos < cur_len) ;
			let input = Rope.sub content (resp_end.pos+1) cur_len in
			let input = Rope.to_string input in
			Log.p "Sending string '%s' to program '%s'" input program ;
			output_string ch_out input ;
			flush ch_out ;
			self#append_prompt
		)
	
	(* Disallow to delete the last prompt *)
	method delete start stop =
		let prompt_stop = resp_end.pos + 1 in
		let prompt_start = prompt_stop - (Rope.length prompt) in
		if start >= prompt_stop || stop <= prompt_start then
			parent#delete start stop

end

(* Create a simple pipe to a shell *)

let shell = new t "/bin/sh"
let _ = new View.text ~append:true shell

