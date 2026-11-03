type color = Red | Black

type node = {
  mutable key: int64;
  mutable val_: int64;
  mutable color: color;
  mutable sz: int;
  mutable left: node option;
  mutable right: node option;
}

let unwrap = function
  | Some x -> x
  | None -> failwith "unwrap"

let is_red = function
  | None -> false
  | Some n -> n.color = Red

let node_size = function
  | None -> 0
  | Some n -> n.sz

let update_size h =
  h.sz <- 1 + node_size h.left + node_size h.right

let rotate_left h =
  let x = unwrap h.right in
  h.right <- x.left;
  x.left <- Some h;
  x.color <- h.color;
  h.color <- Red;
  x.sz <- h.sz;
  update_size h;
  x

let rotate_right h =
  let x = unwrap h.left in
  h.left <- x.right;
  x.right <- Some h;
  x.color <- h.color;
  h.color <- Red;
  x.sz <- h.sz;
  update_size h;
  x

let flip_colors h =
  h.color <- (if h.color = Red then Black else Red);
  let l = unwrap h.left in
  l.color <- (if l.color = Red then Black else Red);
  let r = unwrap h.right in
  r.color <- (if r.color = Red then Black else Red)

let balance h =
  let h_ref = ref h in
  if is_red !h_ref.right && not (is_red !h_ref.left) then
    h_ref := rotate_left !h_ref;
  let hl = !h_ref.left in
  if is_red hl && is_red (match hl with Some n -> n.left | None -> None) then
    h_ref := rotate_right !h_ref;
  if is_red !h_ref.left && is_red !h_ref.right then
    flip_colors !h_ref;
  update_size !h_ref;
  !h_ref

let move_red_left h =
  flip_colors h;
  let hr = unwrap h.right in
  if is_red hr.left then begin
    h.right <- Some (rotate_right hr);
    let h2 = rotate_left h in
    flip_colors h2;
    h2
  end else
    h

let move_red_right h =
  flip_colors h;
  let hl = unwrap h.left in
  if is_red hl.left then begin
    let h2 = rotate_right h in
    flip_colors h2;
    h2
  end else
    h

let rec put_node h key val_ =
  match h with
  | None ->
      Some { key; val_; color = Red; sz = 1; left = None; right = None }
  | Some node ->
      if key < node.key then
        node.left <- put_node node.left key val_
      else if key > node.key then
        node.right <- put_node node.right key val_
      else
        node.val_ <- val_;
      Some (balance node)

let rec min_node h =
  match h.left with
  | None -> h
  | Some l -> min_node l

let rec delete_min h =
  match h.left with
  | None -> None
  | Some l ->
      let h_ref = ref h in
      if not (is_red h.left) && not (is_red l.left) then
        h_ref := move_red_left !h_ref;
      let cur = !h_ref in
      cur.left <- delete_min (unwrap cur.left);
      Some (balance cur)

let rec delete_node h key =
  if key < h.key then begin
    let h_ref = ref h in
    let hl = h.left in
    if not (is_red hl) && not (is_red (match hl with Some n -> n.left | None -> None)) then
      h_ref := move_red_left !h_ref;
    let cur = !h_ref in
    cur.left <- (match cur.left with Some l -> delete_node l key | None -> None);
    Some (balance cur)
  end else begin
    let h_ref = ref h in
    if is_red !h_ref.left then
      h_ref := rotate_right !h_ref;
    let cur = !h_ref in
    if key = cur.key && cur.right = None then
      None
    else begin
      let hr = cur.right in
      if not (is_red hr) && not (is_red (match hr with Some n -> n.left | None -> None)) then
        h_ref := move_red_right !h_ref;
      let cur = !h_ref in
      if key = cur.key then begin
        let m = min_node (unwrap cur.right) in
        let mk = m.key in
        let mv = m.val_ in
        let new_right = delete_min (unwrap cur.right) in
        cur.key <- mk;
        cur.val_ <- mv;
        cur.right <- new_right;
        Some (balance cur)
      end else begin
        cur.right <- (match cur.right with Some r -> delete_node r key | None -> None);
        Some (balance cur)
      end
    end
  end

let put root key val_ =
  root := put_node !root key val_;
  match !root with
  | Some r -> r.color <- Black
  | None -> ()

let get root key =
  let cur = ref !root in
  let res = ref None in
  while !cur <> None && !res = None do
    let n = unwrap !cur in
    if key = n.key then res := Some n.val_
    else if key < n.key then cur := n.left
    else cur := n.right
  done;
  !res

let contains root key =
  get root key <> None

let remove root key =
  if contains root key then begin
    match !root with
    | None -> ()
    | Some r ->
        if not (is_red r.left) && not (is_red r.right) then
          r.color <- Red;
        root := delete_node r key;
        match !root with
        | Some r -> r.color <- Black
        | None -> ()
  end

let rank root key =
  let cur = ref !root in
  let ans = ref 0 in
  while !cur <> None do
    let n = unwrap !cur in
    if key < n.key then
      cur := n.left
    else if key > n.key then begin
      ans := !ans + 1 + node_size n.left;
      cur := n.right
    end else begin
      ans := !ans + node_size n.left;
      cur := None
    end
  done;
  !ans

let range_count root lo hi =
  if lo > hi then 0
  else
    rank root hi - rank root lo + (if contains root hi then 1 else 0)

let floor root key =
  let cur = ref !root in
  let found = ref None in
  while !cur <> None do
    let n = unwrap !cur in
    if key = n.key then begin
      found := Some n.key;
      cur := None
    end else if key < n.key then
      cur := n.left
    else begin
      found := Some n.key;
      cur := n.right
    end
  done;
  !found

let () =
  try
    let q = Scanf.scanf " %d" (fun x -> x) in
    let root = ref None in
    for _ = 1 to q do
      let op = Scanf.scanf " %d" (fun x -> x) in
      if op = 1 then
        let k = Scanf.scanf " %Ld" (fun x -> x) in
        let v = Scanf.scanf " %Ld" (fun x -> x) in
        put root k v
      else if op = 2 then
        let k = Scanf.scanf " %Ld" (fun x -> x) in
        remove root k
      else if op = 3 then
        let k = Scanf.scanf " %Ld" (fun x -> x) in
        match get root k with
        | Some v -> Printf.printf "%Ld\n" v
        | None -> Printf.printf "NONE\n"
      else if op = 4 then
        let lo = Scanf.scanf " %Ld" (fun x -> x) in
        let hi = Scanf.scanf " %Ld" (fun x -> x) in
        Printf.printf "%d\n" (range_count root lo hi)
      else if op = 5 then
        let k = Scanf.scanf " %Ld" (fun x -> x) in
        match floor root k with
        | Some fk -> Printf.printf "%Ld\n" fk
        | None -> Printf.printf "NONE\n"
    done
  with End_of_file -> ()
