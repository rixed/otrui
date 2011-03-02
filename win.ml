open Bricabrac
module Rope = Buf.Rope

type split_dir = Horizontal | Vertical 
let default_split_dir = Horizontal

type way = Up | Down | Left | Right
type up_t = Extend of (way * int * down_t * up_t) | NoExtend
and down_t =
	| Leaf of View.t
	| Split of (split_dir * down_t * int * down_t)
and t = down_t * up_t

let singleton view = Leaf view, NoExtend

let exists f (down, up) =
	let rec exists_down = function
		| Leaf v -> f v
		| Split (_, l, _, r) -> exists_down l || exists_down r
	and exists_up = function
		| NoExtend -> false
		| Extend (_, _, exdown, exup) -> exists_down exdown || exists_up exup in
	exists_down down || exists_up up

let split (down, up) view = function
	| Up    -> Split (Horizontal, Leaf view, 0, down), up
	| Down  -> Split (Horizontal, down, 0, Leaf view), up
	| Left  -> Split (Vertical,   Leaf view, 0, down), up
	| Right -> Split (Vertical,   down, 0, Leaf view), up

(* delete the down tree. *)
let delete = function
	| _, NoExtend -> raise Not_found
	| _, Extend (_, _, down, up) -> down, up

let other_way = function Up -> Down | Down -> Up | Left -> Right | Right -> Left

let display_status left right color x0 y0 width =
	let descr_len  = String.length left
	and status_len = String.length right in
	Term.set_color color ;
	for i = 0 to width - 1 do Term.print (x0+i) y0 Term.hline done ;
	if width >= status_len then (
		Term.print_string (x0+width-status_len) y0 right ;
		let width = width - status_len - 1 in
		if width >= descr_len then
			Term.print_string x0 y0 left
		else
			Term.print_string x0 y0 (String.sub left 0 width)
	)

let global_status_color  = ref (0, false)
let win_status_color     = ref (0, false)
let focused_status_color = ref (0, false)
let vert_split_color     = ref (0, false)

let display_global_status left right y width =
	display_status left right !global_status_color 0 y width 

let display_with_status view x0 y0 width height is_focused =
	assert (height >= 1) ;
	let descr  = view#content_descr
	and status = view#content_status in
	let need_status = descr <> "" || status <> "" in
	if need_status then (
		if height > 1 then view#display x0 y0 width (height-1) is_focused ;
		let color = if is_focused then !focused_status_color else !win_status_color in
		display_status descr status color x0 (y0+height-1) width
	) else view#display x0 y0 width height is_focused

let show_split = ref true

let display_vert_split x0 y0 height =
	Term.set_color !vert_split_color ;
	for y = 0 to (height-1) do
		Term.print x0 (y0+y) Term.vline
	done

let split_size size ds show_split =
	let hs = size/2 in
	let ds = if ds >= hs then hs-1 else if ds <= -hs then (-hs)+1 else ds in
	let ls = hs + ds in
	let ss = if show_split then 1 else 0 in
	let rs = size - ls - ss in
	if ls < 1 then 0, 0, size else if rs < 1 then size, 0, 0 else ls, ss, rs

let widen = function
	| _, NoExtend -> raise Not_found
	| down, Extend (Up, sz, exdown, exup) ->
		Split (Horizontal, exdown, sz, down), exup
	| down, Extend (Down, sz, exdown, exup) ->
		Split (Horizontal, down, sz, exdown), exup
	| down, Extend (Left, sz, exdown, exup) ->
		Split (Vertical, exdown, sz, down), exup
	| down, Extend (Right, sz, exdown, exup) ->
		Split (Vertical, down, sz, exdown), exup

let deepen = function
	| Leaf _, _ -> raise Not_found
	| Split (dir, l, sz, r), up ->
		(* we descend into left window *)
		let way = if dir = Vertical then Right else Down in
		l, Extend (way, sz, r, up)

let rec first_viewable = function
	| Leaf _, _ as x -> x
	| x -> first_viewable (deepen x)

let rec make_outer_root = function
	| down, NoExtend -> down
	| x -> make_outer_root (widen x)

let rec display_down x0 y0 width height focused = function
	| Leaf view as w -> display_with_status view x0 y0 width height (w == focused)
	| Split (dir, l, ds, r) ->
		match dir with
			| Horizontal ->
				let lh, _, rh = split_size height ds false in
				if lh > 0 then display_down x0 y0 width lh focused l ;
				if rh > 0 then display_down x0 (y0+lh) width rh focused r
			| Vertical ->
				let lw, sw, rw = split_size width ds !show_split in
				if lw > 0 then display_down x0 y0 lw height focused l ;
				if sw > 0 then display_vert_split (x0+lw) y0 height ;
				if rw > 0 then display_down (x0+lw+sw) y0 rw height focused r

let display x0 y0 width height ((focused, _) as t) =
	let alldown = make_outer_root t in
	display_down x0 y0 width height focused alldown

let root =
	let root_view = new View.text ~append:true Buf.repl in
	ref (Leaf (root_view :> View.t), NoExtend)

let display_root status_left status_right =
	(* FIXME: views should skip redrawing if not dirty ! *)
	let width, height = Term.screen_size () in
	if height > 1 then display 0 0 width (height-1) !root ;
	display_global_status status_left status_right (height-1) width ;
	Term.redisplay ()

let rec focus_to way = function
	| _, NoExtend -> raise Not_found
	| down, Extend (w, sz, exdown, exup) when w = way ->
		exdown, Extend (other_way w, sz, down, exup)
	| t -> focus_to way (widen t)

let resize way amount (down, up) =
	let rec resize_up = function
		| NoExtend -> raise Not_found
		| Extend (w, sz, exdown, exup) when w = way ->
			let ds = if w = Down || w = Right then amount else -amount in
			Extend (w, sz+ds, exdown, exup)
		| Extend (w, sz, exdown, exup) ->
			Extend (w, sz, exdown, resize_up exup) in
	down, resize_up up

let exchange way (down, up) =
	let rec exchg_up = function
		| NoExtend -> raise Not_found
		| Extend (w, sz, exdown, exup) when w = way ->
			Extend (other_way w, sz, exdown, exup)
		| Extend (w, sz, exdown, exup) ->
			Extend (w, sz, exdown, exchg_up exup) in
	down, exchg_up up

let view_of = function
	| Leaf view, _ -> view
	| Split _, _ -> raise Not_found

let init =
	global_status_color  := Term.get_color (0, 0, 0) (800, 800, 800) ;
	win_status_color     := Term.get_color (0, 0, 0) (900, 900, 900) ;
	focused_status_color := Term.get_color (1000, 1000, 1000) (300, 300, 500) ;
	vert_split_color     := !win_status_color ;

	let content = new Buf.text (Rope.of_file "otrui.ml") "otrui.ml" in
	let v1 = new View.text content
	and v2 = new View.text content in
	v1#set_wrap true ;
	v2#set_wrap false ;
	root := split !root (v1 :> View.t) Down ;
	root := focus_to Down (deepen !root) ;
	root := split !root (v2 :> View.t) Right ;
	root := focus_to Up !root

(* Helpers to manipulate root directly *)

let move_focus_to way   = root := first_viewable (focus_to way !root)
let deepen_focus ()     = root := deepen !root
let widen_focus ()      = root := widen !root
let resize_focus way sz = root := resize way sz !root
let exchange_focus way  = root := exchange way !root
let delete_focus ()     = root := delete !root
let is_focused view     = match !root with Leaf v, _ -> (v :> < >) = (view :> < >) | _ -> false
let is_mapped view      = exists (fun v -> (v :> < >) = (view :> < >)) !root
let set_view view       = match !root with _, up -> root := Leaf view, up
