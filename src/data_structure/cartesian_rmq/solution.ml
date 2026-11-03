let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let a = Array.init n (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
    let left = Array.make n (-1) in
    let right = Array.make n (-1) in
    let parent = Array.make n (-1) in
    let stack = Array.make n 0 in
    let top = ref 0 in
    for i = 0 to n - 1 do
      let last = ref (-1) in
      while !top > 0 && a.(stack.(!top - 1)) > a.(i) do
        decr top;
        last := stack.(!top)
      done;
      if !top > 0 then begin
        right.(stack.(!top - 1)) <- i;
        parent.(i) <- stack.(!top - 1)
      end;
      left.(i) <- !last;
      if !last <> -1 then parent.(!last) <- i;
      stack.(!top) <- i;
      incr top
    done;
    let root = ref 0 in
    for i = 0 to n - 1 do
      if parent.(i) = -1 then root := i
    done;
    let max_tour = max 1 (3 * n) in
    let tour = Array.make max_tour 0 in
    let depth = Array.make max_tour 0 in
    let first = Array.make n (-1) in
    let tour_len = ref 0 in
    let rec euler u d =
      first.(u) <- !tour_len;
      tour.(!tour_len) <- u;
      depth.(!tour_len) <- d;
      incr tour_len;
      if left.(u) <> -1 then begin
        euler left.(u) (d + 1);
        tour.(!tour_len) <- u;
        depth.(!tour_len) <- d;
        incr tour_len
      end;
      if right.(u) <> -1 then begin
        euler right.(u) (d + 1);
        tour.(!tour_len) <- u;
        depth.(!tour_len) <- d;
        incr tour_len
      end
    in
    euler !root 0;
    let m = !tour_len in
    let lg = Array.make (m + 1) 0 in
    for i = 2 to m do
      lg.(i) <- lg.(i / 2) + 1
    done;
    let num_levels = lg.(m) + 1 in
    let st = Array.make_matrix num_levels m 0 in
    for i = 0 to m - 1 do
      st.(0).(i) <- i
    done;
    for k = 1 to num_levels - 1 do
      let half = 1 lsl (k - 1) in
      let len = 1 lsl k in
      for i = 0 to m - len do
        let x = st.(k - 1).(i) in
        let y = st.(k - 1).(i + half) in
        st.(k).(i) <- (if depth.(x) <= depth.(y) then x else y)
      done
    done;
    let q = Scanf.scanf " %d" (fun x -> x) in
    for _ = 1 to q do
      let l = Scanf.scanf " %d" (fun x -> x) in
      let r = Scanf.scanf " %d" (fun x -> x) in
      let i = first.(l) in
      let j = first.(r) in
      let (i, j) = if i > j then (j, i) else (i, j) in
      let k = lg.(j - i + 1) in
      let x = st.(k).(i) in
      let y = st.(k).(j - (1 lsl k) + 1) in
      let best = if depth.(x) <= depth.(y) then x else y in
      Printf.printf "%Ld\n" a.(tour.(best))
    done
  with End_of_file -> ()
