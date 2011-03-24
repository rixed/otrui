type key = int

module type KEY =
sig
	val left      : key
	val right     : key
	val up        : key
	val down      : key
	val ppage     : key
	val npage     : key
	val home      : key
	val end_      : key
	val backspace : key
	val delete    : key
	val escape    : key
	val return    : key

	val get : unit -> key
	(* [get ()] returns the next pressed key code *)

	val to_char : key -> char
	val to_int  : key -> int
	val of_int  : int -> key
end

module type TERM_BASE =
sig
	module Key : KEY

	val print : int -> int -> int -> unit
	(** [print x y c] prints the char c at location x,y *)

	val hline : int
	val vline : int

	type color_pair = int * bool

	val get_color : int * int * int -> int * int * int -> color_pair
	(* [get_color (fg_r, fg_g, fg_b) (bg_r, bg_g, bg_b)] returns the color pair of the described
	 * color *)
	
	val reverse : color_pair -> color_pair

	val set_color : color_pair -> unit
	(* [set_color p] set the color pair p (ie next print will use this color pair) *)
	
	val redraw : unit -> unit

	val screen_size : unit -> int * int

	val beep : unit -> unit

	val quit : unit -> unit
end

module type TERM =
sig
	include TERM_BASE

	val print_string : int -> int -> string -> unit
	(** [print_string x y str] prints the string str at location x,y, clipping in x as necessary *)
end


module type CMD =
sig
	(* FIXME: an abstract type t to replace the int list ? *)
	module Term : TERM
	exception Ambiguous (* When a command may map on several registered commands *)

	val c2i : char -> int
	val to_string : key list -> string

	val register : key list -> ((unit -> unit) -> unit) -> unit
	val to_function : key list -> unit -> unit
end

module Rope =
struct
	include Rope_impl.Make

	type anot = int
	let str2anot = Hashtbl.create 17
	let anot2str = Hashtbl.create 17
	let anot_of_string =
		let seq = ref 0 in (fun str ->
			try Hashtbl.find str2anot str with Not_found ->
				let a = !seq in
				Hashtbl.add str2anot str a ;
				Hashtbl.add anot2str a str ;
				incr seq ; a)
	let string_of_anot a = Hashtbl.find anot2str a
	let none = anot_of_string "none"

	let make_homogen anot t = of_func (length t) (fun i -> nth t i, anot)
	let make_unknown t = make_homogen none t
	let of_string s = of_func (String.length s) (fun i -> s.[i], none)
	let chars_only t = of_func (length t) (fun i -> fst (nth t i))
	let to_string t = to_string (chars_only t)
end

type rope = (char * Rope.anot) Rope.t

module type MARK_BASE =
sig
	type t

	val to_beginning_of   : t -> rope -> unit
	val to_end_of         : t -> rope -> unit
	val update_for_insert : t -> rope -> int -> rope -> unit
	val update_for_cut    : t -> rope -> int -> int -> unit
	val pos               : t -> int
end

type mark =
	{ to_beginning_of   : rope -> unit ;	(* move this mark to the beginning of the given rope *)
	  to_end_of         : rope -> unit ;	(* etc... *)
	  update_for_insert : rope -> int -> rope -> unit ;
	  update_for_cut    : rope -> int -> int -> unit ;
	  pos               : unit -> int }

module type MARK =
sig
	include MARK_BASE

	val mark : t -> mark
end

(* As the same buf may be edited via several views, it must know all the marks
 * used by all the views in order to update all whenever a view updates its
 * rope. But different kind of views need different kind of marks at the same
 * time. So each mark must come with its functions. *)
module type BUF =
sig
	type t

	val mark    : t -> mark -> unit
	val unmark  : t -> mark -> unit

	val content : t -> rope
	val insert  : t -> int -> rope -> unit
	val cut     : t -> int -> int -> unit
	val undo    : t -> unit	(* May raise Not_found *)
	val redo    : t -> unit (* May raise Not_found *)

	val length  : t -> int
	val append  : t -> rope -> unit

	val status  : t -> string

end


module type WIN =
sig
	type split_dir = Horizontal | Vertical
	type way = Up | Down | Left | Right
	type 'a t

	val singleton : 'a -> 'a t
	val root      : 'a t -> 'a option
	val set_root  : 'a t -> 'a -> 'a t
	val widen     : 'a t -> 'a t
	val deepen    : 'a t -> 'a t
	val to_leaf   : 'a t -> 'a t

	val exists    : ('a -> bool) -> 'a t -> bool
	val split     : way -> 'a t -> 'a -> 'a t
	val delete    : 'a t -> 'a t
	(* [delete t] deletes the whole focused tree
	 * or raise Not_found if this would delete the whole t *)
	val focus_to  : way -> 'a t -> 'a t
	val resize    : way -> int -> 'a t -> 'a t
	val exchange  : way -> 'a t -> 'a t

	val iter      : (int -> int -> int -> int -> bool -> 'a -> unit) ->
	                int -> int -> int -> int -> 'a t -> unit
end

module type VIEW_BASE =
sig
	module Term : TERM
	type t

	val draw : t -> int -> int -> int -> int -> bool -> unit
	(* [draw x0 y0 width height focused] redraw the view *)

	val key : t -> key -> unit
	(* [key t k] enter key k into view t *)

	val content_status : t -> string
end

type view =
	{ draw    : int -> int -> int -> int -> bool -> unit ;
	  key     : key -> unit ;
	  descr   : unit -> string ;
	  status  : unit -> string }

module type VIEW =
sig
	include VIEW_BASE

	val view : t -> string -> view
end
