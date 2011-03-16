open Otrui
open Bricabrac

module Make (Mark : MARK_BASE) :
	MARK with type t = Mark.t =
struct
	include Mark

	let mark t =
		{ to_beginning_of   = to_beginning_of t ;
		  to_end_of         = to_end_of t ;
		  update_for_insert = update_for_insert t ;
		  update_for_cut    = update_for_cut t ;
		  pos               = eta pos t }
end
