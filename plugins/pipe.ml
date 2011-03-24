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
	val create : string -> rope -> t
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
		  ch_out      : out_channel ;
		  prompt      : rope }

	let content t = Buf.content t.buf
	let mark t    = Buf.mark t.buf
	let unmark t  = Buf.unmark t.buf
	let append t  = Buf.append t.buf
	let length t  = Buf.length t.buf
	let undo t    = Buf.undo t.buf
	let redo t    = Buf.redo t.buf
	let status t  = Buf.status t.buf

	(* Caller must own Editor.mutex *)
	let append_prompt t =
		let len = length t in
		if t.prompt_mark.pos () < len - (Rope.length t.prompt) then (
			t.prompt_mark.to_end_of (content t) ;
			unmark t t.prompt_mark ;
			append t t.prompt ;
			mark t t.prompt_mark
		) ;
		Buf.reset_undo t.buf

	let create program prompt =
		let reader ch t =
			let aux () =
				let line = input_line ch in
				Mutex.lock mutex ;
				Log.p "Receiving string '%s' from program" line ;
				let line = Rope.cat (Rope.of_string line) (Rope.singleton ('\n', Rope.none)) in
				let pos = t.prompt_mark.pos () in
				Buf.insert t.buf pos line ;
				Buf.reset_undo t.buf ;
				Condition.signal redraw_cond ;
				Mutex.unlock mutex in
			try forever aux ()
			with End_of_file ->
				Log.p "program '%s' exited" program in
		let env = Unix.environment () in
		let ch_in, ch_out, ch_err = Unix.open_process_full program env in
		let t =
			{ buf         = Buf.create () ;
			  prompt_mark = Mark.mark (MarkOffset.create ()) ;
			  ch_out      = ch_out ;
			  prompt      = prompt } in
		t.prompt_mark.to_end_of (content t) ;
		append t prompt ;
		mark t t.prompt_mark ;
		Buf.reset_undo t.buf ;
		ignore (Thread.create (reader ch_in) t) ;
		ignore (Thread.create (reader ch_err) t) ;
		Gc.finalise (fun t ->
			ignore (Unix.close_process_full (ch_in, ch_out, ch_err)) ;
			unmark t t.prompt_mark) t ;
		t

	(* Now redefine insert to send to program stdin *)
	(* Caller must own Editor.mutex *)
	let insert t p c =
		let appending = p = length t in
		Buf.insert t.buf p c ;
		let cur_len = length t in
		if
			appending &&
			cur_len > 0 &&
			fst (Rope.nth (content t) (cur_len - 1)) = '\n'
		then (
			assert (t.prompt_mark.pos () < cur_len) ;
			let input = Rope.sub (content t) (t.prompt_mark.pos () + Rope.length t.prompt) cur_len in
			let input = Rope.to_string input in
			Log.p "Sending string '%s' to program" input ;
			output_string t.ch_out input ;
			flush t.ch_out ;
			append_prompt t
		)

	let exec t str =
		append_prompt t ;
		let cmd = Rope.cat (Rope.of_string str) (Rope.singleton ('\n', Rope.none)) in
		insert t (length t) cmd
	
	(* Disallow to delete the last prompt *)
	let cut t start stop =
		let prompt_start = t.prompt_mark.pos () in
		let prompt_stop = prompt_start + (Rope.length t.prompt) in
		if start >= prompt_stop || stop <= prompt_start then
			Buf.cut t.buf start stop
end

(* Create a simple pipe to a shell (later, implement the "|program" command that forks program and split the current view) *)

module Pipe = Make (Buf)
module Pipe_text_view = View_text.Make (Pipe) (Term) (Cmd)
module Pipe_view = View_impl.Make (Pipe_text_view)

let pipe_buf_of_prog prog prompt =
	Pipe.create prog prompt
let add_and_open_pipe prog prompt =
	let pipe = pipe_buf_of_prog prog prompt in
	let view = Pipe_text_view.create ~append:true pipe in
	Hashtbl.add view.Pipe_text_view.colors (Rope.anot_of_string "prompt") (Term.get_color (800, 500, 1000) (0, 0, 0)) ;
	let v = Pipe_view.view view ("|"^prog) in
	add_and_open_view ("|"^prog) v ;
	v, pipe

let () =
	let shell =
		let a = Rope.anot_of_string "prompt" in
		snd (add_and_open_pipe "/bin/sh" (Rope.of_list [ '>',a; '%',a; ' ',a ])) in
	let shell_exec cmd = Pipe.exec shell (Cmd.to_string cmd) in
	Cmd.register [ Cmd.c2i '|' ] (fun _ -> mode := Dialog ("Shell command", shell_exec)) ;
	Cmd.register (List.map Cmd.c2i ['u';'n';'d';'o']) (fun prev ->
		may !Pipe_text_view.current (fun v -> Pipe_text_view.Buf.undo v.Pipe_text_view.buf) ;
		prev ()) ;
	Cmd.register (List.map Cmd.c2i ['r';'e';'d';'o']) (fun prev ->
		may !Pipe_text_view.current (fun v -> Pipe_text_view.Buf.redo v.Pipe_text_view.buf) ;
		prev ()) ;

