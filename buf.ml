(* Buffers are somewhat editable content. *)
open Bricabrac
module Rope = Rope_impl.Make

class virtual t =
object
	method virtual get : char Rope.t
	method virtual name : string
end

type mark = { mutable pos: int }

class text init_content name =
object (self)
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
	
	(* shortcut *)
	method append c = self#insert (Rope.length content) c

	method delete start stop =
		assert (stop >= start) ;
		content <- Rope.cut content start stop ;
		let update_mark mark =
			if mark.pos >= stop then mark.pos <- mark.pos - (stop-start)
			else if mark.pos >= start then mark.pos <- start in
		List.iter update_mark marks
end

(* REPL *)

(* There's only one repl buffer *)
let repl =
	let prompt = Rope.of_string "# " in
object (self)
	inherit text prompt "REPL" as parent
	
	val mutable resp_end = { pos = 0 }	(* not really *)
	initializer
		resp_end <- parent#mark (Rope.length prompt -1)
	
	method formatter =
		let out str i l = parent#append (Rope.of_func l (fun j -> str.[i+j])) in
		Format.make_formatter out nop

	method append_prompt =
		if resp_end.pos < (Rope.length content) -1 then (
			parent#append prompt ;
			resp_end.pos <- (Rope.length content) -1
		)

	method insert pos c =
		let top_eval cmd =
			let cmd = Rope.to_string cmd in
			Log.p "Executing '%s'" cmd ;
			try
				let l = Lexing.from_string cmd in
				let ph = !Toploop.parse_toplevel_phrase l in
				Toploop.execute_phrase true self#formatter ph
			with exn ->
				(* Idea borrowed from Guillaume Yziquel *)
				let save = !Toploop.parse_use_file in
				Toploop.parse_use_file := (fun _ -> raise exn) ;
				ignore (Toploop.use_silently self#formatter "/dev/null") ;
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
		let appending = pos = Rope.length content in
		parent#insert pos c ;
		if appending && ends_with ";;\n" content then (
			let cur_len = Rope.length content in
			assert (resp_end.pos < cur_len) ;
			let cmd = Rope.sub content (resp_end.pos+1) cur_len in
			let _status = top_eval cmd in
			self#append_prompt
		)
	
	method eval str =
		(* First, append a prompt and moves the cmd start pointer here *)
		self#append_prompt ;
		(* Then append the cmd (with proper termination) *)
		self#append (Rope.of_string str) ;
		self#append (Rope.of_string ";;\n")

	(* Disallow to delete the last prompt *)
	method delete start stop =
		let prompt_stop = resp_end.pos + 1 in
		let prompt_start = prompt_stop - (Rope.length prompt) in
		if start >= prompt_stop || stop <= prompt_start then
			parent#delete start stop
end

let fix_topdirs =
	Log.p "Fixing toplevel directives so that they do not use stdout but repl_fmt" ;
	(* Shadow the useful topdirs that use std_formatter with ones that use repl#formatter *)
	let fmt = repl#formatter in
	Hashtbl.add Toploop.directive_table "use" (Toploop.Directive_string (Topdirs.dir_use fmt)) ;
	Hashtbl.add Toploop.directive_table "load" (Toploop.Directive_string (Topdirs.dir_load fmt)) ;
	Hashtbl.add Toploop.directive_table "trace" (Toploop.Directive_ident (Topdirs.dir_trace fmt)) ;
	Hashtbl.add Toploop.directive_table "untrace" (Toploop.Directive_ident (Topdirs.dir_untrace fmt)) ;
	Hashtbl.add Toploop.directive_table "untrace_all" (Toploop.Directive_none (Topdirs.dir_untrace_all fmt)) ;
(*	Hashtbl.add Toploop.directive_table "warnings" (Toploop.Directive_string (Topdirs.parse_warnings fmt false)) ;
	Hashtbl.add Toploop.directive_table "warn_error" (Toploop.Directive_string (Topdirs.parse_warnings fmt true)) ; *)
	Hashtbl.add Toploop.directive_table "install_printer" (Toploop.Directive_ident (Topdirs.dir_install_printer fmt)) ;
	Hashtbl.add Toploop.directive_table "remove_printer" (Toploop.Directive_ident (Topdirs.dir_remove_printer fmt))

let set_rec_types =
	match Hashtbl.find Toploop.directive_table "rectypes" with
		| Toploop.Directive_none f -> f () ;
		| _ -> failwith "Cannot find rectypes toplevel directive"

let init =
	Toploop.set_paths () ;
	Toploop.initialize_toplevel_env ()


