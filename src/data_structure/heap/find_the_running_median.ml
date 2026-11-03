type heap = {
  data: int array;
  mutable sz: int;
  cmp: int -> int -> bool;
}

let make_heap cap cmp = {
  data = Array.make cap 0;
  sz = 0;
  cmp;
}

let swim h idx =
  let k = ref idx in
  while !k > 0 do
    let p = (!k - 1) / 2 in
    if h.cmp h.data.(!k) h.data.(p) then begin
      let t = h.data.(!k) in
      h.data.(!k) <- h.data.(p);
      h.data.(p) <- t;
      k := p
    end else
      k := 0
  done

let sink h idx =
  let k = ref idx in
  let cont = ref true in
  while !cont do
    let left = 2 * !k + 1 in
    let right = 2 * !k + 2 in
    let best = ref !k in
    if left < h.sz && h.cmp h.data.(left) h.data.(!best) then
      best := left;
    if right < h.sz && h.cmp h.data.(right) h.data.(!best) then
      best := right;
    if !best <> !k then begin
      let t = h.data.(!k) in
      h.data.(!k) <- h.data.(!best);
      h.data.(!best) <- t;
      k := !best
    end else
      cont := false
  done

let push h x =
  let idx = h.sz in
  h.data.(idx) <- x;
  h.sz <- h.sz + 1;
  swim h idx

let pop h =
  let res = h.data.(0) in
  h.sz <- h.sz - 1;
  if h.sz > 0 then begin
    h.data.(0) <- h.data.(h.sz);
    sink h 0
  end;
  res

let top h = h.data.(0)

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let max_heap = make_heap (n + 1) (>) in
    let min_heap = make_heap (n + 1) (<) in
    for _ = 1 to n do
      let v = Scanf.scanf " %d" (fun x -> x) in
      if max_heap.sz = 0 || v <= top max_heap then
        push max_heap v
      else
        push min_heap v;

      if max_heap.sz > min_heap.sz + 1 then
        push min_heap (pop max_heap)
      else if min_heap.sz > max_heap.sz then
        push max_heap (pop min_heap);

      if max_heap.sz = min_heap.sz then
        Printf.printf "%.1f\n" ((float_of_int (top max_heap) +. float_of_int (top min_heap)) /. 2.0)
      else
        Printf.printf "%.1f\n" (float_of_int (top max_heap))
    done
  with End_of_file -> ()
