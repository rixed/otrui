let doc = "This is a plugin for otrui that show how to implement a new kind of buffer.
This one implements a pipe, ie. everything you type in this buffer is send as stdin
to a program, and every output of the program is appended to the buffer."

open Bricabrac
open Buf

let _ = Repl.repl#append (Rope.of_string "Loading pipe buffer\n")

let ignore_sig_child = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

class t program =
	let prompt = Rope.of_string ">% "
	and env = Unix.environment () in
	let ch_in, ch_out, ch_err = Unix.open_process_full program env in
object (self)
	inherit text prompt (Printf.sprintf "|%s" program) as parent

	val mutable resp_end = { pos = 0 }
	val mutable reader_threads = []

	method reader ch =
		let aux () =
			let line = input_line ch in
			Log.p "Receiving string '%s' from program '%s'" line program ;
			let line = Rope.cat (Rope.of_string line) (Rope.singleton '\n') in
			let pos = resp_end.pos + 1 - (Rope.length prompt) in
			parent#insert pos line in
		try forever aux ()
		with End_of_file ->
			Log.p "program '%s' exited" program

	initializer
		resp_end <- parent#mark (Rope.length prompt -1) ;
		Gc.finalise (fun _ -> ignore (Unix.close_process_full (ch_in, ch_out, ch_err))) self ;
		reader_threads <- [
			Thread.create self#reader ch_in ;
			Thread.create self#reader ch_err ]

	method append_prompt =
		if resp_end.pos < (Rope.length content) -1 then (
			parent#append prompt ;
			resp_end.pos <- (Rope.length content) -1
		)

	method insert pos c =
		let appending = pos = Rope.length content in
		parent#insert pos c ;
		if
			appending &&
			Rope.length content > 0 &&
			Rope.nth content ((Rope.length content) -1) = '\n'
		then (
			let cur_len = Rope.length content in
			assert (resp_end.pos < cur_len) ;
			let input = Rope.sub content (resp_end.pos+1) cur_len in
			let input = Rope.to_string input in
			Log.p "Sending string '%s' to program '%s'" input program ;
			try
				output_string ch_out input ;
				flush ch_out ;
				self#append_prompt
			with Sys_error str ->
				Cmd.error str
		)

	method eval str =
		(* First, append a prompt and moves the cmd start pointer here *)
		self#append_prompt ;
		(* Then append the cmd (with proper termination) *)
		self#append (Rope.of_string str) ;
		self#append (Rope.singleton '\n')

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
let install_shell_commands =
	let c2i = Cmd.c2i
	and prev_execute = !Cmd.execute in
	Cmd.execute := function
	(* send a command to the shell *)
	| bang :: cmd when bang = c2i '!' ->
		(try
			let cmd = Cmd.string_of_command cmd in
			shell#eval cmd
		with Invalid_argument _ -> Cmd.error "Cannot execute this 'string'")
	(* unknown command *)
	| x -> prev_execute x

