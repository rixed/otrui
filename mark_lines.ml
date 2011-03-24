open Otrui

(* Implementation of marks with line number + offset from start of line *)

module Make =
struct
	type t =
		{ mutable linepos: int ;
		  mutable lineoff: int ;
		  mutable lineno: int }

	let create () = { linepos = 0 ; lineoff = 0 ; lineno = 0 }

	let pos m = m.linepos + m.lineoff

	let to_beginning_of m _ =
		m.linepos <- 0 ;
		m.lineoff <- 0 ;
		m.lineno <- 0

	let to_end_of m r =
		let r' = Rope.sub r (pos m) (Rope.length r) in
		Log.p "Advance cursor %d times" (Rope.length r') ;
		let pos, off, no =
			Rope.fold_left (fun (p, o, n) c ->
				if fst c = '\n' then p+o+1, 0, n+1 else p, o+1, n)
			(m.linepos, m.lineoff, m.lineno) r' in
		m.linepos <- pos ;
		m.lineoff <- off ;
		m.lineno <- no

	let update_for_insert m _ pos r' =
		let r' = Rope.chars_only r' in
		let nbl = Rope.count r' '\n'
		and len = Rope.length r' in
		if pos < m.linepos then (
			m.linepos <- m.linepos + len ;
			m.lineno  <- m.lineno + nbl
		) else if pos <= m.linepos + m.lineoff then (
			if nbl = 0 then
				m.lineoff <- m.lineoff + len
			else (
				let last_nl = Rope.rindex r' '\n' in
				let dec = pos - m.linepos + last_nl + 1 in
				m.lineno  <- m.lineno + nbl ;
				m.lineoff <- m.linepos + m.lineoff - pos + Rope.length r' - last_nl - 1 ;
				m.linepos <- m.linepos + dec ;
				assert (m.lineoff >= 0)
			)
		)

	let update_for_cut m r start stop =
		Log.p "Cutting %d from %d to %d" (Rope.length r) start stop ;
		let r' = Rope.sub r start stop in
		let r' = Rope.chars_only r' in
		let nbl = Rope.count r' '\n' in
		if m.linepos > stop then (
			m.linepos <- m.linepos - (stop-start) ;
			assert (m.lineno >= nbl) ;
			m.lineno  <- m.lineno - nbl
		) else if m.linepos >= start && m.linepos + m.lineoff >= stop then (
			(* the nbl newlines are before linepos since no newline is allowed between linepos and linepos+lineoff *)
			m.lineoff <- m.linepos + m.lineoff - stop ;
			m.linepos <- start ;
			assert (m.lineno >= nbl) ;
			m.lineno  <- m.lineno - nbl	;
			(* we probably deleted the \n just before linepos. Lets find new beginning of line *)
			let r' = Rope.sub r 0 start in
			let r' = Rope.chars_only r' in
			let last_nl = try Rope.rindex r' '\n' with Not_found -> -1 in
			m.lineoff <- m.lineoff + m.linepos - (last_nl+1) ;
			m.linepos <- last_nl + 1
		) else if m.linepos + m.lineoff >= stop then (
			assert (nbl = 0) ;
			m.lineoff <- m.lineoff - (stop-start)
		) else if m.linepos + m.lineoff >= start then (
			m.lineoff <- start - m.linepos
		)
end

