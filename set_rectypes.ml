let _ =
	match Hashtbl.find Toploop.directive_table "rectypes" with
	| Toploop.Directive_none f -> f () ;
	| _ -> failwith "Cannot find rectypes toplevel directive"

