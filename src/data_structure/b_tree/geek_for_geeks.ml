type btree_node = {
  t: int;
  leaf: bool;
  keys: int array;
  c: btree_node option array;
  mutable n: int;
}

let make_node t leaf =
  {
    t;
    leaf;
    keys = Array.make (2 * t - 1) 0;
    c = Array.make (2 * t) None;
    n = 0;
  }

let unwrap = function
  | Some x -> x
  | None -> failwith "unwrap None"

let rec traverse node =
  for i = 0 to node.n - 1 do
    if not node.leaf then (
      match node.c.(i) with
      | Some child -> traverse child
      | None -> ()
    );
    Printf.printf " %d" node.keys.(i)
  done;
  if not node.leaf then (
    match node.c.(node.n) with
    | Some child -> traverse child
    | None -> ()
  )

let find_key node k =
  let idx = ref 0 in
  while !idx < node.n && node.keys.(!idx) < k do
    incr idx
  done;
  !idx

let split_child parent i y =
  let t = y.t in
  let z = make_node t y.leaf in
  z.n <- t - 1;
  for j = 0 to t - 2 do
    z.keys.(j) <- y.keys.(j + t)
  done;
  if not y.leaf then (
    for j = 0 to t - 1 do
      z.c.(j) <- y.c.(j + t)
    done
  );
  y.n <- t - 1;
  for j = parent.n downto i + 1 do
    parent.c.(j + 1) <- parent.c.(j)
  done;
  parent.c.(i + 1) <- Some z;
  for j = parent.n - 1 downto i do
    parent.keys.(j + 1) <- parent.keys.(j)
  done;
  parent.keys.(i) <- y.keys.(t - 1);
  parent.n <- parent.n + 1

let rec insert_non_full node k =
  let i = ref (node.n - 1) in
  if node.leaf then begin
    while !i >= 0 && node.keys.(!i) > k do
      node.keys.(!i + 1) <- node.keys.(!i);
      decr i
    done;
    node.keys.(!i + 1) <- k;
    node.n <- node.n + 1
  end else begin
    while !i >= 0 && node.keys.(!i) > k do
      decr i
    done;
    let child = unwrap node.c.(!i + 1) in
    if child.n = 2 * node.t - 1 then begin
      split_child node (!i + 1) child;
      if node.keys.(!i + 1) < k then incr i
    end;
    insert_non_full (unwrap node.c.(!i + 1)) k
  end

let insert tree t k =
  match !tree with
  | None ->
      let r = make_node t true in
      r.keys.(0) <- k;
      r.n <- 1;
      tree := Some r
  | Some r ->
      if r.n = 2 * t - 1 then begin
        let s = make_node t false in
        s.c.(0) <- Some r;
        split_child s 0 r;
        let i = if s.keys.(0) < k then 1 else 0 in
        insert_non_full (unwrap s.c.(i)) k;
        tree := Some s
      end else
        insert_non_full r k

let remove_from_leaf node idx =
  for i = idx + 1 to node.n - 1 do
    node.keys.(i - 1) <- node.keys.(i)
  done;
  node.n <- node.n - 1

let get_pred node idx =
  let cur = ref (unwrap node.c.(idx)) in
  while not !cur.leaf do
    cur := unwrap !cur.c.(!cur.n)
  done;
  !cur.keys.(!cur.n - 1)

let get_succ node idx =
  let cur = ref (unwrap node.c.(idx + 1)) in
  while not !cur.leaf do
    cur := unwrap !cur.c.(0)
  done;
  !cur.keys.(0)

let merge node idx =
  let t = node.t in
  let child = unwrap node.c.(idx) in
  let sibling = unwrap node.c.(idx + 1) in
  child.keys.(t - 1) <- node.keys.(idx);
  for i = 0 to sibling.n - 1 do
    child.keys.(i + t) <- sibling.keys.(i)
  done;
  if not child.leaf then (
    for i = 0 to sibling.n do
      child.c.(i + t) <- sibling.c.(i)
    done
  );
  for i = idx + 1 to node.n - 1 do
    node.keys.(i - 1) <- node.keys.(i)
  done;
  for i = idx + 2 to node.n do
    node.c.(i - 1) <- node.c.(i)
  done;
  child.n <- child.n + sibling.n + 1;
  node.n <- node.n - 1

let borrow_from_prev node idx =
  let child = unwrap node.c.(idx) in
  let sibling = unwrap node.c.(idx - 1) in
  for i = child.n - 1 downto 0 do
    child.keys.(i + 1) <- child.keys.(i)
  done;
  if not child.leaf then (
    for i = child.n downto 0 do
      child.c.(i + 1) <- child.c.(i)
    done
  );
  child.keys.(0) <- node.keys.(idx - 1);
  if not child.leaf then
    child.c.(0) <- sibling.c.(sibling.n);
  node.keys.(idx - 1) <- sibling.keys.(sibling.n - 1);
  child.n <- child.n + 1;
  sibling.n <- sibling.n - 1

let borrow_from_next node idx =
  let child = unwrap node.c.(idx) in
  let sibling = unwrap node.c.(idx + 1) in
  child.keys.(child.n) <- node.keys.(idx);
  if not child.leaf then
    child.c.(child.n + 1) <- sibling.c.(0);
  node.keys.(idx) <- sibling.keys.(0);
  for i = 1 to sibling.n - 1 do
    sibling.keys.(i - 1) <- sibling.keys.(i)
  done;
  if not sibling.leaf then (
    for i = 1 to sibling.n do
      sibling.c.(i - 1) <- sibling.c.(i)
    done
  );
  child.n <- child.n + 1;
  sibling.n <- sibling.n - 1

let fill node idx =
  if idx <> 0 && (unwrap node.c.(idx - 1)).n >= node.t then
    borrow_from_prev node idx
  else if idx <> node.n && (unwrap node.c.(idx + 1)).n >= node.t then
    borrow_from_next node idx
  else begin
    if idx <> node.n then
      merge node idx
    else
      merge node (idx - 1)
  end

let rec remove_node node k =
  let idx = find_key node k in
  if idx < node.n && node.keys.(idx) = k then begin
    if node.leaf then
      remove_from_leaf node idx
    else
      remove_from_non_leaf node idx
  end else begin
    if node.leaf then
      Printf.printf "The key %d is does not exist in the tree\n" k
    else begin
      let flag = (idx = node.n) in
      let child = unwrap node.c.(idx) in
      if child.n < node.t then fill node idx;
      if flag && idx > node.n then
        remove_node (unwrap node.c.(idx - 1)) k
      else
        remove_node (unwrap node.c.(idx)) k
    end
  end

and remove_from_non_leaf node idx =
  let k = node.keys.(idx) in
  let c_idx = unwrap node.c.(idx) in
  let c_idx1 = unwrap node.c.(idx + 1) in
  if c_idx.n >= node.t then begin
    let pred = get_pred node idx in
    node.keys.(idx) <- pred;
    remove_node c_idx pred
  end else if c_idx1.n >= node.t then begin
    let succ = get_succ node idx in
    node.keys.(idx) <- succ;
    remove_node c_idx1 succ
  end else begin
    merge node idx;
    remove_node c_idx k
  end

let remove tree k =
  match !tree with
  | None ->
      Printf.printf "The tree is empty\n"
  | Some r ->
      remove_node r k;
      if r.n = 0 then begin
        if r.leaf then tree := None
        else tree := r.c.(0)
      end

let () =
  try
    let d = Scanf.scanf " %d" (fun x -> x) in
    let tree = ref None in
    let rec loop () =
      let op = Scanf.scanf " %s" (fun x -> x) in
      if op = "i" then begin
        let v = Scanf.scanf " %d" (fun x -> x) in
        insert tree d v
      end else if op = "t" then begin
        (match !tree with
         | Some r -> traverse r
         | None -> ());
        print_newline ()
      end else if op = "r" then begin
        let v = Scanf.scanf " %d" (fun x -> x) in
        remove tree v
      end;
      loop ()
    in
    loop ()
  with End_of_file -> ()
