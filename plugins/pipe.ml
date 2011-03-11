(* This is a plugin for otrui that show how to implement a new kind of buffer.
 * This one implements a pipe, ie. everything you type in this buffer is send as stdin
 * to a program, and every output of the program is appended to the buffer. *)

open Bricabrac
open Otrui
open Editor

module Rope = Repl_buf.Rope

let _ = Repl_buf.append_string repl "Loading pipe buffer\n"

let ignore_sig_child = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

module type BUF_PIPE =
sig
	include BUF_BASE
	val exec : t -> string -> unit
end

module Make (Buf : BUF) : BUF_PIPE =
struct
	module Rope = Buf.Rope
	
	type mark = Buf.mark

	type t =
		{ buf            : Buf.t ;
		  resp_end       : mark ;
		  mutex          : Mutex.t ;
		  ch_out         : out_channel }

	let name t    = Buf.name t.buf
	let content t = Buf.content t.buf
	let mark t    = Buf.mark t.buf
	let unmark t  = Buf.unmark t.buf
	let pos       = Buf.pos
	let set_pos   = Buf.set_pos
	let execute t = Buf.execute t.buf

	let prompt = Rope.of_string ">% "

	(* Caller must own mutex *)
	let append_prompt t =
		let len = Buf.length t.buf in
		if pos t.resp_end < len -1 then (
			Buf.append t.buf prompt ;
			set_pos t.resp_end ((Buf.length t.buf) -1)
		)

	let create program content =
		let reader ch t =
			let aux () =
				let line = input_line ch in
				Log.p "Receiving string '%s' from program" line ;
				let line = Rope.cat (Rope.of_string line) (Rope.singleton '\n') in
				with_mutex t.mutex (fun () ->
					let pos = pos t.resp_end + 1 - (Rope.length prompt) in
					Buf.insert t.buf pos line) () in
			try forever aux ()
			with End_of_file ->
				Log.p "program '%s' exited" program in
		let buf = Buf.create ("|"^program) (Rope.cat content prompt) in
		let env = Unix.environment () in
		let ch_in, ch_out, ch_err = Unix.open_process_full program env in
		let rec t =
			{ buf            = buf ;
			  resp_end       = Buf.mark buf (Rope.length prompt -1) ;
			  mutex          = Mutex.create () ;
			  ch_out         = ch_out } in
		ignore (Thread.create (reader ch_in) t) ;
		ignore (Thread.create (reader ch_err) t) ;
		Gc.finalise (fun _ -> ignore (Unix.close_process_full (ch_in, ch_out, ch_err))) t ;
		t

	(* Now redefine insert to send to program stdin *)
	let insert_locked t p c =
		let appending = p = Buf.length t.buf in
		Buf.insert t.buf p c ;
		let cur_len = Buf.length t.buf in
		if
			appending &&
			cur_len > 0 &&
			Rope.nth (content t) (cur_len - 1) = '\n'
		then (
			assert (pos t.resp_end < cur_len) ;
			let input = Rope.sub (content t) (pos t.resp_end + 1) cur_len in
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
			insert_locked t (Buf.length t.buf) cmd) ()
	
	(* Disallow to delete the last prompt *)
	let cut t start stop =
		with_mutex t.mutex (fun () ->
			let prompt_stop = pos t.resp_end + 1 in
			let prompt_start = prompt_stop - (Rope.length prompt) in
			if start >= prompt_stop || stop <= prompt_start then
				Buf.cut t.buf start stop) ()
end

(* Create a simple pipe to a shell (later, implement the "|program" command that forks program and split the current view) *)

module Pipe = Make (Editor.Rope_buf)
module Pipe_buf = Buf_impl.Make (Pipe)
let shell = Pipe_buf.create "/bin/sh" (Pipe_buf.Rope.empty)
module Pipe_text_view = View_text.Make (Pipe_buf) (Editor.Term) (Editor.Cmd)
module Pipe_view = View_impl.Make (Pipe_text_view)
let shell_view = Pipe_text_view.create ~append:true shell
let _ =
	let v = Pipe_view.view shell_view in
	Editor.add_and_open_view "|shell" v

let () =
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

