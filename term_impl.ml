open Otrui

module Make (Term : TERM_BASE) : TERM =
struct
	include Term

	let print_string x y str =
		for i = 0 to (String.length str) - 1 do
			print (x+i) y (int_of_char str.[i])
		done
end
