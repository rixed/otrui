open Otrui

module type S =
sig
	include BUF
	val create : unit -> t
	val reset_undo : t -> unit
end

module Make (Cmd : CMD) : S =
struct

	type modif = Insert of (int * rope) | Cut of (int * int)

	let inverse_modif r = function
		| Insert (p, r') -> Cut (p, p + Rope.length r')
		| Cut (a, b) -> Insert (a, Rope.sub r a b)

	type t =
		{ mutable content : rope ;
		  mutable marks   : mark list ;
		  mutable undos   : modif list ;
		  mutable redos   : modif list }

	let create () =
		{ content = Rope.empty ;
		  marks   = [] ;
		  undos   = [] ;
		  redos   = [] }

	let reset_undo t =
		t.undos <- [] ;
		t.redos <- []

	let content t = t.content

	let mark t m =
		t.marks <- m :: t.marks

	let unmark t m =
		t.marks <- List.filter ((!=) m) t.marks

	let insert t pos c =
		t.undos <- Cut (pos, pos + Rope.length c) :: t.undos ;
		t.content <- Rope.insert t.content pos c ;
		List.iter (fun m -> m.update_for_insert t.content pos c) t.marks
	
	let cut t start stop =
		assert (stop >= start) ;
		List.iter (fun m -> m.update_for_cut t.content start stop) t.marks ;
		Log.p "Cutting '%s' from %d to %d..." (Rope.to_string t.content) start stop ;
		t.undos <- Insert (start, Rope.sub t.content start stop) :: t.undos ;
		t.content <- Rope.cut t.content start stop ;
		Log.p "Got : '%s'" (Rope.to_string t.content)

	let apply_modif t modif = (match modif with
		| Insert (p, c) ->
			Log.p "Applying Insert (%d, \"%s\")" p (Rope.to_string c) ;
			insert t p c
		| Cut (a, b) ->
			Log.p "Applying Cut (%d, %d)" a b ;
			cut t a b) ;
		t.undos <- List.tl t.undos

	let modif_is_continuous = function
		| Insert (p1, r1), Insert (p2, _r2) when p2 = p1 + Rope.length r1 -> true
		| Cut (a1, _b1),    Cut (_a2, b2)   when a1 = b2 -> true
		| _ -> false
	
	let undo t =
		let rec aux modif =
			t.undos <- List.tl t.undos ;
			t.redos <- (inverse_modif t.content modif) :: t.redos ;
			apply_modif t modif ;
			if t.undos <> [] then (
				let next = List.hd t.undos in
				if modif_is_continuous (modif, next) then aux next
			) in
		let modif = try List.hd t.undos with Failure _ -> raise Not_found in
		aux modif

	let redo t =
		let rec aux modif =
			t.redos <- List.tl t.redos ;
			t.undos <- (inverse_modif t.content modif) :: t.undos ;
			apply_modif t modif ;
			if t.redos <> [] then (
				let next = List.hd t.redos in
				if modif_is_continuous (modif, next) then aux next
			) in
		let modif = try List.hd t.redos with Failure _ -> raise Not_found in
		aux modif

	let append t c = insert t (Rope.length (content t)) c
	let append_string t s = insert t (Rope.length (content t)) (Rope.of_string s)
	let length t = Rope.length (content t)
end
