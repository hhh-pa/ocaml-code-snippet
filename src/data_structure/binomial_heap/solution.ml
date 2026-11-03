type binomial_heap = {
  key: int64 array;
  parent: int array;
  child: int array;
  sibling: int array;
  degree: int array;
  mutable sz: int;
  mutable head: int;
}

let make_heap max_nodes =
  let n = max 1 max_nodes in
  {
    key = Array.make n 0L;
    parent = Array.make n (-1);
    child = Array.make n (-1);
    sibling = Array.make n (-1);
    degree = Array.make n 0;
    sz = 0;
    head = -1;
  }

let alloc h x =
  let idx = h.sz in
  h.sz <- h.sz + 1;
  h.key.(idx) <- x;
  h.parent.(idx) <- -1;
  h.child.(idx) <- -1;
  h.sibling.(idx) <- -1;
  h.degree.(idx) <- 0;
  idx

let link h y z =
  h.parent.(y) <- z;
  h.sibling.(y) <- h.child.(z);
  h.child.(z) <- y;
  h.degree.(z) <- h.degree.(z) + 1

let merge_lists h h1 h2 =
  if h1 = -1 then h2
  else if h2 = -1 then h1
  else
    let (head, tail, cur1, cur2) =
      if h.degree.(h1) <= h.degree.(h2) then
        (h1, h1, h.sibling.(h1), h2)
      else
        (h2, h2, h1, h.sibling.(h2))
    in
    let t = ref tail in
    let c1 = ref cur1 in
    let c2 = ref cur2 in
    while !c1 <> -1 && !c2 <> -1 do
      if h.degree.(!c1) <= h.degree.(!c2) then begin
        h.sibling.(!t) <- !c1;
        t := !c1;
        c1 := h.sibling.(!c1)
      end else begin
        h.sibling.(!t) <- !c2;
        t := !c2;
        c2 := h.sibling.(!c2)
      end
    done;
    h.sibling.(!t) <- (if !c1 <> -1 then !c1 else !c2);
    head

let unite h h1 h2 =
  let res_head = merge_lists h h1 h2 in
  if res_head = -1 then -1
  else
    let head = ref res_head in
    let prev = ref (-1) in
    let x = ref !head in
    let next_node = ref h.sibling.(!x) in
    while !next_node <> -1 do
      if h.degree.(!x) <> h.degree.(!next_node) ||
         (h.sibling.(!next_node) <> -1 && h.degree.(h.sibling.(!next_node)) = h.degree.(!x)) then begin
        prev := !x;
        x := !next_node
      end else if h.key.(!x) <= h.key.(!next_node) then begin
        h.sibling.(!x) <- h.sibling.(!next_node);
        link h !next_node !x
      end else begin
        if !prev = -1 then head := !next_node
        else h.sibling.(!prev) <- !next_node;
        link h !x !next_node;
        x := !next_node
      end;
      next_node := h.sibling.(!x)
    done;
    !head

let insert h x =
  let node = alloc h x in
  h.head <- unite h h.head node

let minimum h =
  if h.head = -1 then None
  else
    let min_val = ref h.key.(h.head) in
    let r = ref h.sibling.(h.head) in
    while !r <> -1 do
      if h.key.(!r) < !min_val then min_val := h.key.(!r);
      r := h.sibling.(!r)
    done;
    Some !min_val

let extract_min h =
  if h.head = -1 then None
  else
    let m = ref h.head in
    let mp = ref (-1) in
    let p = ref (-1) in
    let r = ref h.head in
    while !r <> -1 do
      if h.key.(!r) < h.key.(!m) then begin
        m := !r;
        mp := !p
      end;
      p := !r;
      r := h.sibling.(!r)
    done;
    let o = h.key.(!m) in
    if !mp = -1 then h.head <- h.sibling.(!m)
    else h.sibling.(!mp) <- h.sibling.(!m);
    let rev = ref (-1) in
    let c = ref h.child.(!m) in
    while !c <> -1 do
      let nx = h.sibling.(!c) in
      h.sibling.(!c) <- !rev;
      h.parent.(!c) <- -1;
      rev := !c;
      c := nx
    done;
    h.head <- unite h h.head !rev;
    Some o

let () =
  try
    let q = Scanf.scanf " %d" (fun x -> x) in
    let h = make_heap q in
    for _ = 1 to q do
      let op = Scanf.scanf " %d" (fun x -> x) in
      if op = 1 then
        let x = Scanf.scanf " %Ld" (fun x -> x) in
        insert h x
      else if op = 2 then
        match extract_min h with
        | Some o -> Printf.printf "%Ld\n" o
        | None -> Printf.printf "EMPTY\n"
      else if op = 3 then
        match minimum h with
        | Some o -> Printf.printf "%Ld\n" o
        | None -> Printf.printf "EMPTY\n"
    done
  with End_of_file -> ()
