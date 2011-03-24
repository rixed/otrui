open Bricabrac
open Otrui

(* We need a simple mark for our own use *)
module MarkOffset = Mark_offset.Make
module Mark = Mark_impl.Make (MarkOffset)

module Make (Buf : Buf_impl.S) =
struct
	module Buf = Buf

	type t =
		{ buf         : Buf.t ;
		  prompt_mark : mark (* mark the position of the last prompt *) }

	let prompt =
		let a = Rope.anot_of_string "prompt" in
		Rope.of_list [ '#',a ; ' ',a ]
	
	let create () =
		let buf = Buf.create () in
		let prompt_mark = Mark.mark (MarkOffset.create ()) in
		prompt_mark.to_end_of (Buf.content buf) ;
		Buf.append buf prompt ;
		Buf.mark buf prompt_mark ;
		let t = { buf = buf ; prompt_mark = prompt_mark } in
		Gc.finalise (fun t -> Buf.unmark t.buf t.prompt_mark) t ;
		t

	let content t = Buf.content t.buf
	let mark t    = Buf.mark t.buf
	let unmark t  = Buf.unmark t.buf
	let length t  = Buf.length t.buf
	let append t  = Buf.append t.buf
	let undo t    = Buf.undo t.buf
	let redo t    = Buf.redo t.buf
	let status t  = Buf.status t.buf

	let formatter t =
		let out str i l =
			append t (Rope.of_func l (fun j -> str.[i+j], Rope.none)) in
		Format.make_formatter out nop

	let append_prompt t =
		let len = length t in
		if t.prompt_mark.pos () < len - (Rope.length prompt) then (
			t.prompt_mark.to_end_of (content t) ;
			unmark t t.prompt_mark ;
			append t prompt ;
			mark t t.prompt_mark
		)

	(* Now redefine insert to evaluate commands *)
	let insert t p c =
		let top_eval cmd =
			let cmd = Rope.to_string cmd in
			Log.p "Executing '%s'" cmd ;
			try
				let l = Lexing.from_string cmd in
				let ph = !Toploop.parse_toplevel_phrase l in
				Toploop.execute_phrase true (formatter t) ph
			with exn ->
				(* Idea borrowed from Guillaume Yziquel *)
				let save = !Toploop.parse_use_file in
				Toploop.parse_use_file := (fun _ -> raise exn) ;
				ignore (Toploop.use_silently (formatter t) "/dev/null") ;
				Toploop.parse_use_file := save ;
				false
		in
		let ends_with e r =
			let lr = Rope.length r in
			let le = String.length e in
			if lr >= le then (
				let e' = Rope.to_string (Rope.sub r (lr-le) lr) in
				e' = e
			) else false in
		let appending = p = length t in
		Buf.insert t.buf p c ;
		if appending && ends_with ";;\n" (content t) then (
			let cur_len = length t in
			Log.p "cur_len = %d while prompt_mark is at %d" cur_len (t.prompt_mark.pos ());
			assert (t.prompt_mark.pos () < cur_len) ;
			let cmd = Rope.sub (content t) (t.prompt_mark.pos () + Rope.length prompt) cur_len in
			let _status = top_eval cmd in
			append_prompt t ;
			Buf.reset_undo t.buf
		)

	let eval t str =
		(* First, append a prompt *)
		append_prompt t ;
		(* Then append the cmd (with proper termination) *)
		append t (Rope.of_string str) ;
		insert t (length t) (Rope.of_string ";;\n")

	(* Disallow to delete the last prompt *)
	let cut t start stop =
		let prompt_start = t.prompt_mark.pos () in
		let prompt_stop = prompt_start + Rope.length prompt in
		if start >= prompt_stop || stop <= prompt_start then
			Buf.cut t.buf start stop
	
	let grab_topdirs t =
		Log.p "Fixing toplevel directives so that they do not use stdout but repl_fmt" ;
		let fmt = formatter t in
		Hashtbl.replace Toploop.directive_table "use" (Toploop.Directive_string (Topdirs.dir_use fmt)) ;
		Hashtbl.replace Toploop.directive_table "load" (Toploop.Directive_string (Topdirs.dir_load fmt)) ;
		Hashtbl.replace Toploop.directive_table "trace" (Toploop.Directive_ident (Topdirs.dir_trace fmt)) ;
		Hashtbl.replace Toploop.directive_table "untrace" (Toploop.Directive_ident (Topdirs.dir_untrace fmt)) ;
		Hashtbl.replace Toploop.directive_table "untrace_all" (Toploop.Directive_none (Topdirs.dir_untrace_all fmt)) ;
	(*	Hashtbl.replace Toploop.directive_table "warnings" (Toploop.Directive_string (Topdirs.parse_warnings fmt false)) ;
		Hashtbl.replace Toploop.directive_table "warn_error" (Toploop.Directive_string (Topdirs.parse_warnings fmt true)) ; *)
		Hashtbl.replace Toploop.directive_table "install_printer" (Toploop.Directive_ident (Topdirs.dir_install_printer fmt)) ;
		Hashtbl.replace Toploop.directive_table "remove_printer" (Toploop.Directive_ident (Topdirs.dir_remove_printer fmt)) ;

end

let init =
	Log.p "Forcing rectypes" ;
	(match Hashtbl.find Toploop.directive_table "rectypes" with
		| Toploop.Directive_none f -> f () ;
		| _ -> failwith "Cannot find rectypes toplevel directive") ;
	Log.p "Initializing Toploop" ;
	Toploop.set_paths () ;
	Toploop.initialize_toplevel_env ()

