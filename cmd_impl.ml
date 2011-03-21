open Otrui

module Make (Term : TERM) :
	CMD with module Term = Term =
struct
	module Term = Term

	exception Unknown
	exception Error of string

	let error str = raise (Error str)

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
		let unk () = raise Unknown in
		let rec aux root = function
			| [] -> failwith "Cannot add an empty command"
			| [k] -> Hashtbl.add root k (Hashtbl.create 11, f)
			| k :: rest ->
				let next_root, _ = try Hashtbl.find root k with Not_found -> (
					let n = Hashtbl.create 11, unk in
					Hashtbl.add root k n ; n
				) in
				aux next_root rest in
		aux cmd_tree cmd

	let to_function cmd =
		let rec aux root = function
			| [] ->
				raise Unknown (* TODO *)
			| [k] ->
				let _, f = try Hashtbl.find root k with Not_found -> raise Unknown in f
			| k :: rest ->
				let next_root, _ = try Hashtbl.find root k with Not_found -> raise Unknown in
				aux next_root rest in
		aux cmd_tree cmd
end
