(* This is a plugin for otrui that show how to implement a new kind of buffer.
 * This one implements a pipe, ie. everything you type in this buffer is send as stdin
 * to a program, and every output of the program is appended to the buffer. *)

open Bricabrac
open Otrui
open Editor

let _ = Repl.append repl (Rope.of_string "Loading pipe buffer\n")

let ignore_sig_child = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

module type S =
sig
	include BUF
	module Buf : BUF
	val create : string -> char Rope.t -> t
	(* [create prog prompt] creates a pipe to program prog, using given prompt *)
	val exec   : t -> string -> unit
end

(* We need a simple mark for our own use *)
module MarkOffset = Mark_offset.Make
module Mark = Mark_impl.Make (MarkOffset)

module Make (Buf : Buf_impl.S) :
	S with module Buf = Buf =
struct
	module Buf  = Buf

	type t =
		{ buf         : Buf.t ;
		  prompt_mark : mark ;
		  mutex       : Mutex.t ;
		  ch_out      : out_channel ;
		  prompt      : char Rope.t }

	let content t = Buf.content t.buf
	let mark t    = Buf.mark t.buf
	let unmark t  = Buf.unmark t.buf
	let execute t = Buf.execute t.buf
	let append t  = Buf.append t.buf
	let length t  = Buf.length t.buf

	(* Caller must own mutex *)
	let append_prompt t =
		let len = length t in
		if t.prompt_mark.pos () < len - (Rope.length t.prompt) then (
			t.prompt_mark.to_end_of (content t) ;
			unmark t t.prompt_mark ;
			append t t.prompt ;
			mark t t.prompt_mark
		)

	let create program prompt =
		let reader ch t =
			let aux () =
				let line = input_line ch in
				Log.p "Receiving string '%s' from program" line ;
				let line = Rope.cat (Rope.of_string line) (Rope.singleton '\n') in
				with_mutex t.mutex (fun () ->
					let pos = t.prompt_mark.pos () + 1 - (Rope.length t.prompt) in
					Buf.insert t.buf pos line) () in
			try forever aux ()
			with End_of_file ->
				Log.p "program '%s' exited" program in
		let env = Unix.environment () in
		let ch_in, ch_out, ch_err = Unix.open_process_full program env in
		let t =
			{ buf         = Buf.create () ;
			  prompt_mark = Mark.mark (MarkOffset.create ()) ;
			  mutex       = Mutex.create () ;
			  ch_out      = ch_out ;
			  prompt      = prompt } in
		t.prompt_mark.to_end_of (content t) ;
		append t prompt ;
		mark t t.prompt_mark ;
		ignore (Thread.create (reader ch_in) t) ;
		ignore (Thread.create (reader ch_err) t) ;
		Gc.finalise (fun t ->
			ignore (Unix.close_process_full (ch_in, ch_out, ch_err)) ;
			unmark t t.prompt_mark) t ;
		t

	(* Now redefine insert to send to program stdin *)
	let insert_locked t p c =
		let appending = p = length t in
		Buf.insert t.buf p c ;
		let cur_len = length t in
		if
			appending &&
			cur_len > 0 &&
			Rope.nth (content t) (cur_len - 1) = '\n'
		then (
			assert (t.prompt_mark.pos () < cur_len) ;
			let input = Rope.sub (content t) (t.prompt_mark.pos () + Rope.length t.prompt) cur_len in
			let input = Rope.to_string input in
			Log.p "Sending string '%s' to program" input ;
			try
				output_string t.ch_out input ;
				flush t.ch_out ;
				append_prompt t ;
			with Sys_error str ->
				Cmd.error str
		)

	let insert t p c = with_mutex t.mutex (insert_locked t p) c

	let exec t str =
		with_mutex t.mutex (fun () ->
			append_prompt t ;
			let cmd = Rope.cat (Rope.of_string str) (Rope.singleton '\n') in
			insert_locked t (length t) cmd) ()
	
	(* Disallow to delete the last prompt *)
	let cut t start stop =
		with_mutex t.mutex (fun () ->
			let prompt_start = t.prompt_mark.pos () in
			let prompt_stop = prompt_start + (Rope.length t.prompt) in
			if start >= prompt_stop || stop <= prompt_start then
				Buf.cut t.buf start stop) ()
end

(* Create a simple pipe to a shell (later, implement the "|program" command that forks program and split the current view) *)

module Pipe = Make (Editor.Buf)
module Pipe_text_view = View_text.Make (Pipe) (Editor.Term) (Editor.Cmd)
module Pipe_view = View_impl.Make (Pipe_text_view)

let pipe_buf_of_prog prog prompt =
	Pipe.create prog (Rope.of_string prompt)
let add_and_open_pipe prog prompt =
	let pipe = pipe_buf_of_prog prog prompt in
	let view = Pipe_text_view.create ~append:true pipe in
	let v = Pipe_view.view view ("|"^prog) in
	Editor.add_and_open_view ("|"^prog) v

let () =
	let shell = pipe_buf_of_prog "/bin/sh" ">% " in
	let shell_view = Pipe_text_view.create ~append:true shell in
	let v = Pipe_view.view shell_view "|shell" in
	Editor.add_and_open_view "|shell" v ;
	let c2i = Cmd.c2i
	and prev_execute = !Cmd.execute in
	Cmd.execute := function
	(* send a command to the shell *)
	| bang :: cmd when bang = c2i '!' ->
		(try
			let cmd = Cmd.string_of_command cmd in
			Pipe.exec shell cmd
		with Invalid_argument _ -> Cmd.error "Cannot execute this 'string'")
	(* unknown command *)
	| x -> prev_execute x

