open Bricabrac
module Rope = Buf.Rope

let fix_topdirs () =
	Log.p "Fixing toplevel directives so that they do not use stdout but repl_fmt" ;
	(* Shaddow the usefull topdirs that use std_formatter with ones that use repl#formatter *)
	let fmt = Buf.repl#formatter in
	Hashtbl.add Toploop.directive_table "use" (Toploop.Directive_string (Topdirs.dir_use fmt)) ;
	Hashtbl.add Toploop.directive_table "load" (Toploop.Directive_string (Topdirs.dir_load fmt)) ;
	Hashtbl.add Toploop.directive_table "trace" (Toploop.Directive_ident (Topdirs.dir_trace fmt)) ;
	Hashtbl.add Toploop.directive_table "untrace" (Toploop.Directive_ident (Topdirs.dir_untrace fmt)) ;
	Hashtbl.add Toploop.directive_table "untrace_all" (Toploop.Directive_none (Topdirs.dir_untrace_all fmt)) ;
(*	Hashtbl.add Toploop.directive_table "warnings" (Toploop.Directive_string (Topdirs.parse_warnings fmt false)) ;
	Hashtbl.add Toploop.directive_table "warn_error" (Toploop.Directive_string (Topdirs.parse_warnings fmt true)) ; *)
	Hashtbl.add Toploop.directive_table "install_printer" (Toploop.Directive_ident (Topdirs.dir_install_printer fmt)) ;
	Hashtbl.add Toploop.directive_table "remove_printer" (Toploop.Directive_ident (Topdirs.dir_remove_printer fmt))

let start =
	fix_topdirs () ;
	Term.init () ;
	Buf.init () ;
	View.init () ;
	Win.init () ;
	Cmd.init () ;
	let instdir = "." in
	Buf.repl#eval ("#load \""^instdir^"/system.cmo\"") ;
	Buf.repl#eval ("#use \""^(Unix.getenv "HOME")^".otrui.rc\"") ;
	Buf.repl#eval "#use \"./.otrui.rc\"" ;
	Cmd.key_loop ()

