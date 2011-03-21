open Otrui
open Bricabrac

module Make (View : VIEW_BASE) :
	VIEW with type t = View.t and module Term = View.Term =
struct
	include View

	let view t name =
		{ draw    = draw t ;
		  key     = key t ;
		  descr   = (fun () -> name) ;
		  status  = eta content_status t }
end
