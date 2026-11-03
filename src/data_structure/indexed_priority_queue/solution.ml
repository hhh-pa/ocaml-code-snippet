type index_min_pq = {
  n: int;
  mutable sz: int;
  pq: int array;
  qp: int array;
  keys: int64 array;
}

let make_pq n = {
  n;
  sz = 0;
  pq = Array.make (n + 1) 0;
  qp = Array.make n (-1);
  keys = Array.make n 0L;
}

let less pq i j =
  if pq.keys.(i) <> pq.keys.(j) then
    pq.keys.(i) < pq.keys.(j)
  else
    i < j

let exch pq a b =
  let t = pq.pq.(a) in
  pq.pq.(a) <- pq.pq.(b);
  pq.pq.(b) <- t;
  pq.qp.(pq.pq.(a)) <- a;
  pq.qp.(pq.pq.(b)) <- b

let swim pq idx =
  let k = ref idx in
  while !k > 1 && less pq pq.pq.(!k) pq.pq.(!k / 2) do
    exch pq !k (!k / 2);
    k := !k / 2
  done

let sink pq idx =
  let k = ref idx in
  let cont = ref true in
  while 2 * !k <= pq.sz && !cont do
    let j = ref (2 * !k) in
    if !j < pq.sz && less pq pq.pq.(!j + 1) pq.pq.(!j) then
      incr j;
    if not (less pq pq.pq.(!j) pq.pq.(!k)) then
      cont := false
    else begin
      exch pq !k !j;
      k := !j
    end
  done

let change pq i key =
  pq.keys.(i) <- key;
  swim pq pq.qp.(i);
  sink pq pq.qp.(i)

let insert pq i key =
  if pq.qp.(i) <> -1 then
    change pq i key
  else begin
    pq.sz <- pq.sz + 1;
    pq.qp.(i) <- pq.sz;
    pq.pq.(pq.sz) <- i;
    pq.keys.(i) <- key;
    swim pq pq.sz
  end

let has pq i =
  i >= 0 && i < pq.n && pq.qp.(i) <> -1

let min_index pq = pq.pq.(1)

let del_min pq =
  let m = pq.pq.(1) in
  exch pq 1 pq.sz;
  pq.sz <- pq.sz - 1;
  sink pq 1;
  pq.qp.(m) <- -1;
  m

let erase pq i =
  let k = pq.qp.(i) in
  exch pq k pq.sz;
  pq.sz <- pq.sz - 1;
  swim pq k;
  sink pq k;
  pq.qp.(i) <- -1

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let q = Scanf.scanf " %d" (fun x -> x) in
    let pq = make_pq n in
    for _ = 1 to q do
      let op = Scanf.scanf " %d" (fun x -> x) in
      if op = 1 then
        let idx = Scanf.scanf " %d" (fun x -> x) in
        let k = Scanf.scanf " %Ld" (fun x -> x) in
        if idx >= 0 && idx < n then insert pq idx k
      else if op = 2 then
        if pq.sz = 0 then Printf.printf "EMPTY\n"
        else Printf.printf "%d\n" (min_index pq)
      else if op = 3 then
        if pq.sz = 0 then Printf.printf "EMPTY\n"
        else Printf.printf "%d\n" (del_min pq)
      else if op = 4 then
        let idx = Scanf.scanf " %d" (fun x -> x) in
        if has pq idx then begin
          erase pq idx;
          Printf.printf "OK\n"
        end else
          Printf.printf "NONE\n"
      else if op = 5 then
        let idx = Scanf.scanf " %d" (fun x -> x) in
        if has pq idx then
          Printf.printf "%Ld\n" pq.keys.(idx)
        else
          Printf.printf "NONE\n"
    done
  with End_of_file -> ()
