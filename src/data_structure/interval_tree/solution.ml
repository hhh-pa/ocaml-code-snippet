type node =
  | Nil
  | Node of {
      lo: int64;
      hi: int64;
      id: int64;
      max_hi: int64;
      height: int;
      left: node;
      right: node;
    }

let height = function
  | Nil -> 0
  | Node { height = h; _ } -> h

let max_hi = function
  | Nil -> -0x7fffffffffffffffL
  | Node { max_hi = m; _ } -> m

let make_node lo hi id l r =
  let hl = height l in
  let hr = height r in
  let h = 1 + max hl hr in
  let m1 = max hi (max_hi l) in
  let m = max m1 (max_hi r) in
  Node { lo; hi; id; max_hi = m; height = h; left = l; right = r }

let rotate_left = function
  | Node { lo = x_lo; hi = x_hi; id = x_id; left = x_l; right = Node { lo = y_lo; hi = y_hi; id = y_id; left = y_l; right = y_r; _ }; _ } ->
      let new_x = make_node x_lo x_hi x_id x_l y_l in
      make_node y_lo y_hi y_id new_x y_r
  | u -> u

let rotate_right = function
  | Node { lo = y_lo; hi = y_hi; id = y_id; left = Node { lo = x_lo; hi = x_hi; id = x_id; left = x_l; right = x_r; _ }; right = y_r; _ } ->
      let new_y = make_node y_lo y_hi y_id x_r y_r in
      make_node x_lo x_hi x_id x_l new_y
  | u -> u

let balance u =
  match u with
  | Nil -> Nil
  | Node { left = l; right = r; lo; hi; id; _ } ->
      let bf = height l - height r in
      if bf > 1 then
        match l with
        | Node { left = ll; right = lr; _ } ->
            let l' = if height ll < height lr then rotate_left l else l in
            rotate_right (make_node lo hi id l' r)
        | Nil -> u
      else if bf < -1 then
        match r with
        | Node { left = rl; right = rr; _ } ->
            let r' = if height rr < height rl then rotate_right r else r in
            rotate_left (make_node lo hi id l r')
        | Nil -> u
      else
        make_node lo hi id l r

let cmp_key lo1 hi1 id1 lo2 hi2 id2 =
  if lo1 <> lo2 then compare lo1 lo2
  else if hi1 <> hi2 then compare hi1 hi2
  else compare id1 id2

let rec insert_node u lo hi id =
  match u with
  | Nil -> make_node lo hi id Nil Nil
  | Node { lo = u_lo; hi = u_hi; id = u_id; left = l; right = r; _ } ->
      let c = cmp_key lo hi id u_lo u_hi u_id in
      if c < 0 then balance (make_node u_lo u_hi u_id (insert_node l lo hi id) r)
      else if c > 0 then balance (make_node u_lo u_hi u_id l (insert_node r lo hi id))
      else u

let rec remove_min = function
  | Nil -> failwith "remove_min Nil"
  | Node { left = Nil; right = r; lo; hi; id; _ } -> (lo, hi, id, r)
  | Node { left = l; right = r; lo; hi; id; _ } ->
      let (min_lo, min_hi, min_id, l') = remove_min l in
      (min_lo, min_hi, min_id, balance (make_node lo hi id l' r))

let rec remove_node u lo hi id =
  match u with
  | Nil -> Nil
  | Node { lo = u_lo; hi = u_hi; id = u_id; left = l; right = r; _ } ->
      let c = cmp_key lo hi id u_lo u_hi u_id in
      if c < 0 then balance (make_node u_lo u_hi u_id (remove_node l lo hi id) r)
      else if c > 0 then balance (make_node u_lo u_hi u_id l (remove_node r lo hi id))
      else
        match l, r with
        | Nil, Nil -> Nil
        | Nil, _ -> r
        | _, Nil -> l
        | _, _ ->
            let (succ_lo, succ_hi, succ_id, r') = remove_min r in
            balance (make_node succ_lo succ_hi succ_id l r')

let rec query_node u qlo qhi acc =
  match u with
  | Nil -> acc
  | Node { lo; hi; id; max_hi = mhi; left = l; right = r; _ } ->
      if mhi < qlo then acc
      else
        let acc1 = query_node l qlo qhi acc in
        let acc2 = if lo <= qhi && qlo <= hi then id :: acc1 else acc1 in
        if lo <= qhi then query_node r qlo qhi acc2
        else acc2

let () =
  try
    let q = Scanf.scanf " %d" (fun x -> x) in
    let root = ref Nil in
    let by_id = Hashtbl.create 16 in
    for _ = 1 to q do
      let t = Scanf.scanf " %d" (fun x -> x) in
      if t = 1 then begin
        let lo = Scanf.scanf " %Ld" (fun x -> x) in
        let hi = Scanf.scanf " %Ld" (fun x -> x) in
        let id = Scanf.scanf " %Ld" (fun x -> x) in
        (match Hashtbl.find_opt by_id id with
         | Some (old_lo, old_hi) -> root := remove_node !root old_lo old_hi id
         | None -> ());
        Hashtbl.replace by_id id (lo, hi);
        root := insert_node !root lo hi id
      end else if t = 2 then begin
        let id = Scanf.scanf " %Ld" (fun x -> x) in
        (match Hashtbl.find_opt by_id id with
         | Some (lo, hi) ->
             Hashtbl.remove by_id id;
             root := remove_node !root lo hi id
         | None -> ())
      end else if t = 3 then begin
        let qlo = Scanf.scanf " %Ld" (fun x -> x) in
        let qhi = Scanf.scanf " %Ld" (fun x -> x) in
        let res = List.sort compare (query_node !root qlo qhi []) in
        if res = [] then
          Printf.printf "NONE\n"
        else begin
          List.iteri (fun i v ->
            if i > 0 then print_string " ";
            Printf.printf "%Ld" v
          ) res;
          print_newline ()
        end
      end
    done
  with End_of_file -> ()
