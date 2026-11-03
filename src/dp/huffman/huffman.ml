type node = {
  freq: int64;
  min_sym: int;
  id: int;
}

let cmp_node a b =
  if a.freq <> b.freq then
    if a.freq < b.freq then -1 else 1
  else if a.min_sym <> b.min_sym then
    if a.min_sym < b.min_sym then -1 else 1
  else
    compare a.id b.id

type heap = {
  mutable data: node array;
  mutable size: int;
}

let heap_create cap =
  { data = Array.make cap { freq = 0L; min_sym = 0; id = 0 }; size = 0 }

let heap_push h x =
  if h.size = Array.length h.data then begin
    let next = Array.make (h.size * 2) { freq = 0L; min_sym = 0; id = 0 } in
    Array.blit h.data 0 next 0 h.size;
    h.data <- next
  end;
  let i = ref h.size in
  h.size <- h.size + 1;
  while !i > 0 && cmp_node x h.data.((!i - 1) / 2) < 0 do
    h.data.(!i) <- h.data.((!i - 1) / 2);
    i := (!i - 1) / 2
  done;
  h.data.(!i) <- x

let heap_pop h =
  let ret = h.data.(0) in
  h.size <- h.size - 1;
  if h.size > 0 then begin
    let x = h.data.(h.size) in
    let i = ref 0 in
    let half = h.size / 2 in
    let cont = ref true in
    while !i < half && !cont do
      let left = 2 * !i + 1 in
      let right = left + 1 in
      let best =
        if right < h.size && cmp_node h.data.(right) h.data.(left) < 0 then right
        else left
      in
      if cmp_node h.data.(best) x < 0 then begin
        h.data.(!i) <- h.data.(best);
        i := best
      end else
        cont := false
    done;
    h.data.(!i) <- x
  end;
  ret

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let fr = Array.init n (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
    if n = 1 then begin
      Printf.printf "0\n0\n"
    end else begin
      let max_nodes = 2 * n + 5 in
      let left_child = Array.make max_nodes (-1) in
      let right_child = Array.make max_nodes (-1) in
      let h = heap_create (2 * n + 10) in
      for i = 0 to n - 1 do
        heap_push h { freq = fr.(i); min_sym = i; id = i }
      done;
      let cur_id = ref n in
      while h.size > 1 do
        let n1 = heap_pop h in
        let n2 = heap_pop h in
        let sum_f = Int64.add n1.freq n2.freq in
        let min_m = min n1.min_sym n2.min_sym in
        let nid = !cur_id in
        incr cur_id;
        left_child.(nid) <- n1.id;
        right_child.(nid) <- n2.id;
        heap_push h { freq = sum_f; min_sym = min_m; id = nid }
      done;
      let total_nodes = !cur_id in
      let root = (heap_pop h).id in
      let dd = Array.make total_nodes 0 in
      let st = Stack.create () in
      Stack.push root st;
      while not (Stack.is_empty st) do
        let u = Stack.pop st in
        if left_child.(u) <> -1 then begin
          dd.(left_child.(u)) <- dd.(u) + 1;
          dd.(right_child.(u)) <- dd.(u) + 1;
          Stack.push left_child.(u) st;
          Stack.push right_child.(u) st
        end
      done;
      let cost = ref 0L in
      for i = 0 to n - 1 do
        cost := Int64.add !cost (Int64.mul fr.(i) (Int64.of_int dd.(i)))
      done;
      Printf.printf "%Ld\n" !cost;
      for i = 0 to n - 1 do
        if i > 0 then print_string " ";
        print_int dd.(i)
      done;
      print_newline ()
    end
  with End_of_file -> ()
