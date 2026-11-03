let inf = 1000000000

type min_heap = {
  mutable data : (int * int) array;
  mutable sz : int;
}

let make_heap cap = {
  data = Array.make cap (0, 0);
  sz = 0;
}

let push h x =
  if h.sz >= Array.length h.data then begin
    let new_data = Array.make (max 1 (2 * Array.length h.data)) (0, 0) in
    Array.blit h.data 0 new_data 0 h.sz;
    h.data <- new_data
  end;
  let idx = h.sz in
  h.sz <- h.sz + 1;
  let k = ref idx in
  while !k > 0 do
    let p = (!k - 1) / 2 in
    if compare x h.data.(p) < 0 then begin
      h.data.(!k) <- h.data.(p);
      k := p
    end else
      k := 0
  done;
  h.data.(!k) <- x

let pop h =
  let res = h.data.(0) in
  h.sz <- h.sz - 1;
  if h.sz > 0 then begin
    let x = h.data.(h.sz) in
    let k = ref 0 in
    let cont = ref true in
    while !cont do
      let left = 2 * !k + 1 in
      let right = 2 * !k + 2 in
      let best = ref !k in
      if left < h.sz && compare h.data.(left) x < 0 then best := left;
      let cmp_val = if !best = !k then x else h.data.(!best) in
      if right < h.sz && compare h.data.(right) cmp_val < 0 then best := right;
      if !best <> !k then begin
        h.data.(!k) <- h.data.(!best);
        k := !best
      end else
        cont := false
    done;
    h.data.(!k) <- x
  end;
  res

let top h = h.data.(0)
let is_empty h = h.sz = 0

let solve_case () =
  let n = Scanf.scanf " %d" (fun x -> x) in
  let m = Scanf.scanf " %d" (fun x -> x) in
  let s = Scanf.scanf " %d" (fun x -> x) in
  let g = Scanf.scanf " %d" (fun x -> x) in
  let adj = Array.make n [] in
  for _ = 0 to m - 1 do
    let u = Scanf.scanf " %d" (fun x -> x) in
    let v = Scanf.scanf " %d" (fun x -> x) in
    let w = Scanf.scanf " %d" (fun x -> x) in
    adj.(u) <- (v, w) :: adj.(u);
    adj.(v) <- (u, w) :: adj.(v)
  done;
  for i = 0 to n - 1 do
    adj.(i) <- List.sort compare adj.(i)
  done;

  (* Uniform-cost search *)
  let dist = Array.make n inf in
  let par = Array.make n (-1) in
  dist.(s) <- 0;
  let pq = make_heap 16 in
  push pq (0, s);
  let stop = ref false in
  while not (is_empty pq) && not !stop do
    let (d, v) = pop pq in
    if d = dist.(v) then begin
      if v = g then stop := true
      else begin
        List.iter (fun (to_n, w) ->
          let nd = d + w in
          if nd < dist.(to_n) then begin
            dist.(to_n) <- nd;
            par.(to_n) <- v;
            push pq (nd, to_n)
          end
        ) adj.(v)
      end
    end
  done;

  if dist.(g) >= inf then
    Printf.printf "UCS -1\n"
  else begin
    let path = ref [] in
    let v = ref g in
    while !v <> -1 do
      path := !v :: !path;
      v := par.(!v)
    done;
    Printf.printf "UCS %d" dist.(g);
    List.iter (fun x -> Printf.printf " %d" x) !path;
    print_newline ()
  end;

  (* Bidirectional Dijkstra *)
  let ds = Array.make n inf in
  let dg = Array.make n inf in
  let ps = Array.make n (-1) in
  let pg = Array.make n (-1) in
  let vs = Array.make n false in
  let vg = Array.make n false in
  ds.(s) <- 0;
  dg.(g) <- 0;
  vs.(s) <- true;
  vg.(g) <- true;
  let qs = make_heap 16 in
  let qg = make_heap 16 in
  push qs (0, s);
  push qg (0, g);
  let best = ref inf in
  let meet = ref (-1) in
  let turn = ref 0 in
  let finished = ref false in
  while not !finished do
    while not (is_empty qs) && fst (top qs) <> ds.(snd (top qs)) do
      ignore (pop qs)
    done;
    while not (is_empty qg) && fst (top qg) <> dg.(snd (top qg)) do
      ignore (pop qg)
    done;
    if is_empty qs && is_empty qg then finished := true
    else begin
      let ts = if is_empty qs then inf else fst (top qs) in
      let tg = if is_empty qg then inf else fst (top qg) in
      if !best < inf && ts + tg >= !best then finished := true
      else begin
        if !turn mod 2 = 0 then begin
          if not (is_empty qs) then begin
            let (d, v) = pop qs in
            if d = ds.(v) then begin
              vs.(v) <- true;
              if vg.(v) && ds.(v) + dg.(v) < !best then begin
                best := ds.(v) + dg.(v);
                meet := v
              end;
              List.iter (fun (to_n, w) ->
                let nd = d + w in
                if nd < ds.(to_n) then begin
                  ds.(to_n) <- nd;
                  ps.(to_n) <- v;
                  vs.(to_n) <- true;
                  push qs (nd, to_n);
                  if vg.(to_n) && nd + dg.(to_n) < !best then begin
                    best := nd + dg.(to_n);
                    meet := to_n
                  end
                end
              ) adj.(v)
            end
          end
        end else begin
          if not (is_empty qg) then begin
            let (d, v) = pop qg in
            if d = dg.(v) then begin
              vg.(v) <- true;
              if vs.(v) && ds.(v) + dg.(v) < !best then begin
                best := ds.(v) + dg.(v);
                meet := v
              end;
              List.iter (fun (to_n, w) ->
                let nd = d + w in
                if nd < dg.(to_n) then begin
                  dg.(to_n) <- nd;
                  pg.(to_n) <- v;
                  vg.(to_n) <- true;
                  push qg (nd, to_n);
                  if vs.(to_n) && ds.(to_n) + nd < !best then begin
                    best := ds.(to_n) + nd;
                    meet := to_n
                  end
                end
              ) adj.(v)
            end
          end
        end;
        incr turn
      end
    end
  done;

  if !meet < 0 then
    Printf.printf "BIDIR -1\n"
  else begin
    let left = ref [] in
    let v = ref !meet in
    while !v <> -1 do
      left := !v :: !left;
      v := ps.(!v)
    done;
    let right = ref [] in
    v := pg.(!meet);
    while !v <> -1 do
      right := !v :: !right;
      v := pg.(!v)
    done;
    let right = List.rev !right in
    Printf.printf "BIDIR %d" !best;
    List.iter (fun x -> Printf.printf " %d" x) !left;
    List.iter (fun x -> Printf.printf " %d" x) right;
    print_newline ()
  end

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
