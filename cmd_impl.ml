open Otrui
open Bricabrac

module Make (Term : TERM) :
	CMD with module Term = Term =
struct
	module Term = Term

	exception Ambiguous

	let c2i = int_of_char

	let to_string cmd =
		let len = List.length cmd in
		let str = String.create len in
		let rec aux i = function
			| [] -> str
			| c::cmd ->
				str.[i] <- Term.Key.to_char c ;
				aux (i+1) cmd in
		aux 0 cmd

	let cmd_tree = Hashtbl.create 11	(* key -> subhash * f_for_key *)

	let register cmd f =
		Log.p "Registering command for '%s'" (to_string cmd) ;
		let rec aux root = function
			| [] -> failwith "Cannot add an empty command"
			| [k] -> Hashtbl.add root k (Hashtbl.create 11, Some f)
			| k :: rest ->
				let next_root, _ = try Hashtbl.find root k with Not_found -> (
					let n = Hashtbl.create 11, None in
					Hashtbl.add root k n ; n
				) in
				aux next_root rest in
		aux cmd_tree cmd

	let to_function cmd =
		let rec aux root last_f = function
			| [] ->
				let l = Hashtbl.length root in
				if l = 0 then
					match last_f with Some f -> f | None -> raise Not_found
				else if l = 1 then (
					let next_root, f = List.hd (hashtbl_values root) in
					aux next_root f []
				) else raise Ambiguous
			| k :: rest ->
				let next_root, f = Hashtbl.find root k in
				aux next_root f rest in
		aux cmd_tree None cmd
end
