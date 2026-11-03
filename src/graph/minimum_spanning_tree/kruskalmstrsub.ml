let rec find parent x =
  if parent.(x) = x then x
  else begin
    let root = find parent parent.(x) in
    parent.(x) <- root;
    root
  end

let union parent size x y =
  let rx = find parent x in
  let ry = find parent y in
  if rx = ry then false
  else begin
    let r1, r2 =
      if size.(rx) < size.(ry) then (ry, rx) else (rx, ry)
    in
    parent.(r2) <- r1;
    size.(r1) <- size.(r1) + size.(r2);
    true
  end

let () =
  try
    let g_nodes = Scanf.scanf " %d" (fun x -> x) in
    let g_edges = Scanf.scanf " %d" (fun x -> x) in
    let edges = Array.init g_edges (fun _ ->
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      let w = Scanf.scanf " %Ld" (fun x -> x) in
      (w, u, v)
    ) in
    Array.sort (fun (w1, u1, v1) (w2, u2, v2) ->
      let c = compare (w1 : int64) w2 in
      if c <> 0 then c
      else
        let c2 = compare (u1 : int) u2 in
        if c2 <> 0 then c2
        else compare (v1 : int) v2
    ) edges;
    let parent = Array.init (g_nodes + 1) (fun i -> i) in
    let size = Array.make (g_nodes + 1) 1 in
    let min_weight = ref 0L in
    Array.iter (fun (w, u, v) ->
      if union parent size u v then
        min_weight := Int64.add !min_weight w
    ) edges;
    Printf.printf "%Ld\n" !min_weight
  with End_of_file -> ()
