type key = int

module type KEY =
sig
	val left      : key
	val right     : key
	val up        : key
	val down      : key
	val ppage     : key
	val npage     : key
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
	exception Unknown         (* When a command is destined to somebody else *)
	exception Error of string (* When a command execution fails (error string is displayed) *)
	val error : string -> unit

	val c2i : char -> int
	val string_of_command : key list -> string

	type execute_fun = key list -> unit
	val execute : execute_fun ref
end


module type BUF_BASE =
sig
	module Rope : Pfds_intf.ROPE_GEN

	type t
	type mark

	val create  : string -> char Rope.t -> t
	(* [create name content] creates the named buf with given initial content *)
	val name    : t -> string
	val content : t -> char Rope.t
	val mark    : t -> int -> mark
	val unmark  : t -> mark -> unit
	val pos     : mark -> int
	val set_pos : mark -> int -> unit

	val insert  : t -> int -> char Rope.t -> unit
	val cut     : t -> int -> int -> unit

	val execute : t -> int list -> unit
	(* [execute t cmd] executes the cmd on t or raise Cmd.Unknown *)
end

module type BUF =
sig
	include BUF_BASE

	val append_string : t -> string -> unit
	val append        : t -> char Rope.t -> unit
	val length        : t -> int
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

	val execute : t -> int list -> unit
	(* [execute t cmd] executes the cmd on t or raise Cmd.Unknown *)

	val content_descr : t -> string
	val content_status : t -> string
end

type view =
	{ draw    : int -> int -> int -> int -> bool -> unit ;
	  key     : key -> unit ;
	  execute : int list -> unit ;
	  descr   : unit -> string ;
	  status  : unit -> string }

module type VIEW =
sig
	include VIEW_BASE

	val view : t -> view
end
