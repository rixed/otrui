exception Unknown         (* When a command is destined to somebody else *)
exception Error of string (* When a command execution fails (error string is displayed) *)

let error str = raise (Error str)

let c2i = int_of_char

let string_of_command cmd =
	let len = List.length cmd in
	let str = String.create len in
	let rec aux i = function [] -> str | c::cmd -> str.[i] <- char_of_int c ; aux (i-1) cmd in
	aux (len-1) cmd

type execute_fun = int list -> unit
let execute = ref ((fun _ -> error "Unknown command") : execute_fun)

