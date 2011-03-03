(* Compile with:
 * ocamlfind ocamlc -linkall -o eval.byte toplevellib.cma eval.ml
 *)

let top_eval cmd =
	try
		let l = Lexing.from_string cmd in
		let ph = !Toploop.parse_toplevel_phrase l in
		Toploop.execute_phrase true Format.std_formatter ph
	with exn ->
		let save = !Toploop.parse_use_file in
		Toploop.parse_use_file := (fun _ -> raise exn) ;
		ignore (Toploop.use_silently Format.std_formatter "/dev/null") ;
		Toploop.parse_use_file := save ;
		false

let set_rec_types =
	match Hashtbl.find Toploop.directive_table "rectypes" with
		| Toploop.Directive_none f -> f () ;
		| _ -> failwith "Cannot find rectypes toplevel directive"

let init =
	Toploop.set_paths () ;
	Toploop.initialize_toplevel_env ()

let main =
	for i = 1 to (Array.length Sys.argv) -1 do
		ignore (top_eval Sys.argv.(i))
	done
