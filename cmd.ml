open Bricabrac

(* Command parse and execute *)

exception Error of string
let error str = raise (Error str)

let c2i = int_of_char

let default_commands = function
	(* quit *)
	| [ q ] when q = c2i 'q' ->
		Log.p "Quit" ;
		Term.quit () ; exit 0
	(* unrecognized command *)
	| _ -> error "Unknown command"

let execute = ref (default_commands : int list -> unit)

let rec execute_times ?count = function
	(* nop *)
	| [] -> ()
	(* repetition count *)
	| c :: rest when c >= c2i '0' && c <= c2i '9' ->
		execute_times ~count:((optdef count 0)*10 + c - (c2i '0')) rest
	| cmd ->
		for c = 1 to (optdef count 1) do !execute cmd done

