let rec gcdll a b =
  let a = if a < 0L then Int64.neg a else a in
  let b = if b < 0L then Int64.neg b else b in
  if b = 0L then (if a = 0L then 1L else a)
  else gcdll b (Int64.rem a b)

type fr = { p : int64; q : int64 }

let norm p q =
  let p, q = if q < 0L then (Int64.neg p, Int64.neg q) else (p, q) in
  let g = gcdll p q in
  { p = Int64.div p g; q = Int64.div q g }

let mul a b = norm (Int64.mul a.p b.p) (Int64.mul a.q b.q)
let add a b = norm (Int64.add (Int64.mul a.p b.q) (Int64.mul b.p a.q)) (Int64.mul a.q b.q)

type factor = {
  mutable scope : int list;
  mutable tab : fr array;
}

let solve_case () =
  let n = Scanf.scanf " %d" (fun x -> x) in
  let parents = Array.make n [] in
  let cpt = Array.make n [||] in
  for v = 0 to n - 1 do
    let k = Scanf.scanf " %d" (fun x -> x) in
    let pars = Array.init k (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    parents.(v) <- Array.to_list pars;
    let rows = 1 lsl k in
    let cpt_rows = Array.init rows (fun _ ->
      let num = Scanf.scanf " %Ld" (fun x -> x) in
      let den = Scanf.scanf " %Ld" (fun x -> x) in
      norm num den
    ) in
    cpt.(v) <- cpt_rows
  done;
  let q = Scanf.scanf " %d" (fun x -> x) in
  let ne = Scanf.scanf " %d" (fun x -> x) in
  let evid = Array.make n (-1) in
  for _ = 0 to ne - 1 do
    let e = Scanf.scanf " %d" (fun x -> x) in
    let vv = Scanf.scanf " %d" (fun x -> x) in
    evid.(e) <- vv
  done;

  let facs = ref [] in
  for v = 0 to n - 1 do
    let sc = List.sort compare (v :: parents.(v)) in
    let s = List.length sc in
    let sz = 1 lsl s in
    let sc_arr = Array.of_list sc in
    let tab = Array.make sz { p = 0L; q = 1L } in
    let k = List.length parents.(v) in
    let pars_arr = Array.of_list parents.(v) in
    for i = 0 to sz - 1 do
      let vv = ref 0 in
      for j = 0 to s - 1 do
        let bit = (i lsr (s - 1 - j)) land 1 in
        if sc_arr.(j) = v then vv := bit
      done;
      let row = ref 0 in
      for j = 0 to k - 1 do
        let pj = pars_arr.(j) in
        let pos = ref 0 in
        while sc_arr.(!pos) <> pj do incr pos done;
        let bit = (i lsr (s - 1 - !pos)) land 1 in
        row := !row lor (bit lsl (k - 1 - j))
      done;
      let pr = cpt.(v).(!row) in
      tab.(i) <- if !vv = 1 then pr else norm (Int64.sub pr.q pr.p) pr.q
    done;
    facs := { scope = sc; tab } :: !facs
  done;
  let facs_list = ref (List.rev !facs) in

  (* Restrict evidence *)
  List.iter (fun f ->
    for e = 0 to n - 1 do
      if evid.(e) >= 0 then begin
        let pos = ref (-1) in
        let sc_arr = Array.of_list f.scope in
        for j = 0 to Array.length sc_arr - 1 do
          if sc_arr.(j) = e then pos := j
        done;
        if !pos >= 0 then begin
          let s = Array.length sc_arr in
          let sz = 1 lsl s in
          let ns = List.filter (fun x -> x <> e) f.scope in
          let nt = ref [] in
          for i = 0 to sz - 1 do
            let bit = (i lsr (s - 1 - !pos)) land 1 in
            if bit = evid.(e) then nt := f.tab.(i) :: !nt
          done;
          f.scope <- ns;
          f.tab <- Array.of_list (List.rev !nt)
        end
      end
    done
  ) !facs_list;

  (* Eliminate hidden variables *)
  for v = 0 to n - 1 do
    if v <> q && evid.(v) < 0 then begin
      let has = ref [] in
      let rest = ref [] in
      List.iter (fun f ->
        if List.mem v f.scope then has := f :: !has
        else rest := f :: !rest
      ) !facs_list;
      if !has <> [] then begin
        let sc_set = Hashtbl.create 16 in
        List.iter (fun f -> List.iter (fun x -> Hashtbl.replace sc_set x ()) f.scope) !has;
        let sc = List.sort compare (Hashtbl.fold (fun k () acc -> k :: acc) sc_set []) in
        let s = List.length sc in
        let sz = 1 lsl s in
        let sc_arr = Array.of_list sc in
        let tab = Array.make sz { p = 1L; q = 1L } in
        for i = 0 to sz - 1 do
          let pr = ref { p = 1L; q = 1L } in
          List.iter (fun f ->
            let idx = ref 0 in
            let f_sz = List.length f.scope in
            let f_sc_arr = Array.of_list f.scope in
            for j = 0 to f_sz - 1 do
              let pos = ref 0 in
              while sc_arr.(!pos) <> f_sc_arr.(j) do incr pos done;
              let bit = (i lsr (s - 1 - !pos)) land 1 in
              idx := !idx lor (bit lsl (f_sz - 1 - j))
            done;
            pr := mul !pr f.tab.(!idx)
          ) !has;
          tab.(i) <- !pr
        done;

        let pos = ref 0 in
        while sc_arr.(!pos) <> v do incr pos done;
        let ns = List.filter (fun x -> x <> v) sc in
        let nt = Array.make (1 lsl (s - 1)) { p = 0L; q = 1L } in
        for i = 0 to sz - 1 do
          let ni = ref 0 in
          for j = 0 to s - 1 do
            if j <> !pos then begin
              let bit = (i lsr (s - 1 - j)) land 1 in
              ni := (!ni lsl 1) lor bit
            end
          done;
          nt.(!ni) <- add nt.(!ni) tab.(i)
        done;
        facs_list := { scope = ns; tab = nt } :: !rest
      end
    end
  done;

  let sc_set = Hashtbl.create 16 in
  List.iter (fun f -> List.iter (fun x -> Hashtbl.replace sc_set x ()) f.scope) !facs_list;
  let sc = List.sort compare (Hashtbl.fold (fun k () acc -> k :: acc) sc_set []) in
  let sn = List.length sc in
  let szn = 1 lsl (max sn 1) in
  let sc_arr = Array.of_list sc in
  let jt = ref { p = 0L; q = 1L } in
  let jf = ref { p = 0L; q = 1L } in
  for i = 0 to szn - 1 do
    let pr = ref { p = 1L; q = 1L } in
    List.iter (fun f ->
      let idx = ref 0 in
      let f_sz = List.length f.scope in
      let f_sc_arr = Array.of_list f.scope in
      for j = 0 to f_sz - 1 do
        let ppos = ref 0 in
        while sc_arr.(!ppos) <> f_sc_arr.(j) do incr ppos done;
        let bit = (i lsr (sn - 1 - !ppos)) land 1 in
        idx := !idx lor (bit lsl (f_sz - 1 - j))
      done;
      pr := mul !pr f.tab.(!idx)
    ) !facs_list;
    let ppos = ref 0 in
    while !ppos < sn && sc_arr.(!ppos) <> q do incr ppos done;
    if !ppos < sn then begin
      let bit = (i lsr (sn - 1 - !ppos)) land 1 in
      if bit = 1 then jt := add !jt !pr
      else jf := add !jf !pr
    end
  done;

  let tot = add !jt !jf in
  let ans =
    if tot.p = 0L then { p = 0L; q = 1L }
    else norm (Int64.mul !jt.p tot.q) (Int64.mul !jt.q tot.p)
  in
  Printf.printf "VE %Ld/%Ld\n" ans.p ans.q

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
