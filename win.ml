module Rope = Buf.Rope

type split_dir = Horizontal | Vertical 
type split_size = Absolute of int (* min abs size *) | Relative of float

type win =
	| Leaf of View.t
	| Split of (split_dir * (split_size * win) list)

let win_sub_sizes children =
	let rec aux abs rel = function
		| [] -> abs, rel
		| (Absolute a, _)::l -> aux (abs+a) rel l
		| (Relative r, _)::l -> aux abs (rel+.r) l in
	aux 0 0. children

let default_split_dir = Horizontal

let split ?(split_dir=default_split_dir) view size win =
	Split (split_dir, [ size, Leaf view ; Relative 1., win ])

let resplit view size = function
	| Leaf _ -> failwith "Cannot resplit leaf window"
	| Split (split_dir, children) ->
		Split (split_dir, (size, Leaf view) :: children)

let size_of rem_abs = function
	| Absolute a -> min a rem_abs
	| Relative r -> int_of_float ((float_of_int rem_abs) *. r)

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

let win_status_color = ref (Term.Color.green, Term.Color.magenta)
let display_win_status view x0 y0 width =
	let descr  = view#content_descr
	and status = view#content_status in
	display_status descr status !win_status_color x0 y0 width

let global_status_color = ref (Term.Color.blue, Term.Color.black)
let display_global_status left right y width =
	display_status left right !global_status_color 0 y width 

let display_with_status view x0 y0 width height =
	assert (height >= 1) ;
	if height > 1 then view#display x0 y0 width (height-1) ;
	display_win_status view x0 (y0+height-1) width

let show_vert_split = ref true
let vert_split_color = ref (Term.Color.green, Term.Color.magenta)
let rec display x0 y0 width height = function
	| Leaf view -> display_with_status view x0 y0 width height
	| Split (dir, children) ->
		let rec aux c = function
			| [] -> failwith "should not happen"
			| [_, win] ->
				let x0, y0, width, height = match dir with
				| Horizontal -> x0, c, width, height - c
				| Vertical -> c, y0, width - c, height in
				if width > 0 && height > 0 then display x0 y0 width height win
			| (sz, win) :: children' ->
				let x0, y0, width, height = match dir with
				| Horizontal -> x0, c, width, size_of (height - c) sz
				| Vertical -> c, y0, size_of (width - c) sz, height in
				if width > 0 && height > 0 then (
					if dir = Vertical && !show_vert_split then (
						if width > 1 then display x0 y0 (width-1) height win ;
						Term.set_color !vert_split_color ;
						for y = 0 to (height-1) do
							Term.print (x0+width-1) (y0+y) Term.vline
						done
					) else (
						display x0 y0 width height win
					)
				) ;
				let next_c = match dir with Horizontal -> y0 + height | Vertical -> x0 + width in
				aux next_c children' in
		aux 0 children

let root =
	let content = new Buf.text (Rope.of_file "test.ml") "test.ml" in
	let v1 = new View.text "view1" content
	and v2 = new View.text "view2" content in
	v1#set_wrap ~symbol_color:(Term.Color.yellow, Term.Color.black) true ;
	v2#set_wrap ~symbol_color:(Term.Color.yellow, Term.Color.black) false ;
	let top = split ~split_dir:Vertical (v2:>View.t) (Relative 0.5) (Leaf (v1:>View.t)) in
	let repl_view = new View.text "repl" ~append:true Buf.repl in
	split (repl_view:>View.t) (Absolute 10) top

let display_root status_left status_right =
	let width, height = Term.screen_size () in
	if height > 1 then display 0 0 width (height-1) root ;
	display_global_status status_left status_right (height-1) width ;
	Term.redisplay ()

