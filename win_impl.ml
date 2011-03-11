open Bricabrac
open Otrui

module Make : WIN =
struct
	type split_dir = Horizontal | Vertical
	type way = Up | Down | Left | Right

	type 'a up_t = Extend of (way * int * 'a down_t * 'a up_t) | NoExtend
	and 'a down_t = Leaf of 'a | Split of (split_dir * 'a down_t * int * 'a down_t)
	and 'a t = 'a down_t * 'a up_t

	let singleton e = Leaf e, NoExtend

	let root = function
		| Leaf r, _ -> Some r
		| _ -> None

	let set_root (_, up) r = Leaf r, up

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

	let rec to_leaf = function
		| Leaf _, _ as x -> x
		| x -> to_leaf (deepen x)

	let exists f (down, up) =
		let rec exists_down = function
			| Leaf v -> f v
			| Split (_, l, _, r) -> exists_down l || exists_down r
		and exists_up = function
			| NoExtend -> false
			| Extend (_, _, exdown, exup) -> exists_down exdown || exists_up exup in
		exists_down down || exists_up up

	let split way (down, up) e = match way with
		| Up    -> Split (Horizontal, Leaf e, 0, down), up
		| Down  -> Split (Horizontal, down, 0, Leaf e), up
		| Left  -> Split (Vertical,   Leaf e, 0, down), up
		| Right -> Split (Vertical,   down, 0, Leaf e), up

	(* delete the down tree. *)
	let delete = function
		| _, NoExtend -> raise Not_found
		| _, Extend (_, _, down, up) -> down, up

	let other_way = function Up -> Down | Down -> Up | Left -> Right | Right -> Left

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

	let split_size size ds =
		let hs = size/2 in
		let ds = if ds >= hs then hs-1 else if ds <= -hs then (-hs)+1 else ds in
		let ls = hs + ds in
		let rs = size - ls in
		if ls < 1 then 0, size else if rs < 1 then size, 0 else ls, rs

	let rec make_outer_root = function
		| down, NoExtend -> down
		| x -> make_outer_root (widen x)

	let iter f x0 y0 width height t =
		let focused, _ = t in
		let all_down = make_outer_root t in
		let rec iter_down x0 y0 width height = function
			| Leaf e as d -> f x0 y0 width height (focused == d) e
			| Split (dir, l, ds, r) ->
				match dir with
					| Horizontal ->
						let lh, rh = split_size height ds in
						if lh > 0 then iter_down x0 y0 width lh l ;
						if rh > 0 then iter_down x0 (y0+lh) width rh r
					| Vertical ->
						let lw, rw = split_size width ds in
						if lw > 0 then iter_down x0 y0 lw height l ;
						if rw > 0 then iter_down (x0+lw) y0 rw height r in
		iter_down x0 y0 width height all_down

end
