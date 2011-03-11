open Otrui
open Bricabrac

module Make (View : VIEW_BASE) :
	VIEW with type t = View.t and module Term = View.Term =
struct
	include View

	let view t =
		{ draw    = draw t ;
		  key     = key t ;
		  execute = execute t ;
		  descr   = eta content_descr t ;
		  status  = eta content_status t }
end
