let focused =
	let rec get_first_view = function
		| [] -> failwith "No views?"
		| (_, Win.Leaf view) :: _ -> view
		| (_, Win.Split (_, children)) :: l -> get_first_view (children @ l) in
	get_first_view [Win.Relative 1., Win.root]

let key_loop () =
	Log.p "Loop for getch" ;
	let rec next_key () =
		Win.display_root () ;
		let k = Term.key () in
		if k = 27 (* ESC *) then (
			Term.quit () ;
			exit 0
		) else (
			focused#key k ;
			next_key ()
		) in
	next_key ()
