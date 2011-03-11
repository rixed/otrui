open Otrui

module Make (Term : TERM) :
	CMD with module Term = Term =
struct
	module Term = Term

	exception Unknown
	exception Error of string

	let error str = raise (Error str)

	let c2i = int_of_char

	let string_of_command cmd =
		let len = List.length cmd in
		let str = String.create len in
		let rec aux i = function
			| [] -> str
			| c::cmd ->
				str.[i] <- Term.Key.to_char c ;
				aux (i+1) cmd in
		aux 0 cmd

	type execute_fun = int list -> unit
	let execute = ref ((fun _ -> error "Unknown command") : execute_fun)
end
