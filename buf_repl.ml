open Bricabrac
open Otrui

module type S =
sig
	include BUF
	module Buf : BUF
	val create       : unit -> t
	val eval         : t -> string -> unit
	val grab_topdirs : t -> unit
end

module Make (Buf : Buf_impl.S) :
	S with module Buf = Buf and type mark = Buf.mark =
struct
	module Buf = Buf

	type mark = Buf.mark

	type t =
		{ buf      : Buf.t ;
		  resp_end : mark }

	let prompt = Rope.of_string "# "
	
	let create () =
		let buf = Buf.create () in
		Buf.append buf prompt ;
		{ buf      = buf ;
		  resp_end = Buf.mark buf (Rope.length prompt -1) }

	let content t = Buf.content t.buf
	let mark t    = Buf.mark t.buf
	let unmark t  = Buf.unmark t.buf
	let pos       = Buf.pos
	let set_pos   = Buf.set_pos
	let execute t = Buf.execute t.buf
	let length t  = Buf.length t.buf
	let append t  = Buf.append t.buf

	let formatter t =
		let out str i l =
			Buf.append t.buf (Rope.of_func l (fun j -> str.[i+j])) in
		Format.make_formatter out nop

	let append_prompt t =
		let len = length t in
		if pos t.resp_end < len -1 then (
			Buf.append t.buf prompt ;
			set_pos t.resp_end (length t -1)
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
			Log.p "cur_len = %d while resp_end is at %d" cur_len (pos t.resp_end) ;
			assert (pos t.resp_end < cur_len) ;
			let cmd = Rope.sub (content t) ((pos t.resp_end) + 1) cur_len in
			let _status = top_eval cmd in
			append_prompt t
		)

	let eval t str =
		(* First, append a prompt and moves the cmd start pointer here *)
		append_prompt t ;
		(* Then append the cmd (with proper termination) *)
		Buf.append t.buf (Rope.of_string str) ;
		insert t (length t) (Rope.of_string ";;\n")

	(* Disallow to delete the last prompt *)
	let cut t start stop =
		let prompt_stop = (pos t.resp_end) + 1 in
		let prompt_start = prompt_stop - (Rope.length prompt) in
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

