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

	let cmds = ref []

	let starts_with prefix =
		let rec same_start c1 c2 = match c1, c2 with
			| [], _ -> true
			| k1 :: k1', (k2 :: k2') when k1 = k2 ->
				same_start k1' k2'
			| _ -> false in
		List.filter (fun (c, _f) -> same_start prefix c) !cmds

	let register cmd f =
		Log.p "Registering command for '%s'" (to_string cmd) ;
		cmds := (cmd, f) :: !cmds

	let to_function cmd =
		let rec all_same ((last_c, _last_f) as last) = function
			| [] -> true
			| (c, _f) :: rest when c = last_c -> all_same last rest
			| _ -> false in
		let cmds = starts_with cmd in
		if cmds = [] then raise Not_found ;
		if not (all_same (List.hd cmds) (List.tl cmds)) then raise Ambiguous ;
		let rec call_next rest () = match rest with
			| [] -> ()
			| (_, f) :: rest' -> f (call_next rest') in
		call_next cmds
end
