type edge = { u: int; v: int; w: int64; idx: int }

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let edges = Array.init m (fun i ->
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      let w = Scanf.scanf " %Ld" (fun x -> x) in
      { u; v; w; idx = i }
    ) in
    let parent = Array.init n (fun i -> i) in
    let rec find_root i =
      if parent.(i) = i then i
      else begin
        parent.(i) <- find_root parent.(i);
        parent.(i)
      end
    in
    let unite a b =
      let ra = find_root a in
      let rb = find_root b in
      if ra = rb then false
      else begin
        parent.(rb) <- ra;
        true
      end
    in
    let edge_indices = Array.init m (fun i -> i) in
    Array.sort (fun i j ->
      let c = Int64.compare edges.(i).w edges.(j).w in
      if c <> 0 then c
      else compare i j
    ) edge_indices;
    let total_weight = ref 0L in
    let edges_count = ref 0 in
    let mst_edges = ref [] in
    for k = 0 to m - 1 do
      let idx = edge_indices.(k) in
      let e = edges.(idx) in
      if unite e.u e.v then begin
        total_weight := Int64.add !total_weight e.w;
        incr edges_count;
        let e_min = min e.u e.v in
        let e_max = max e.u e.v in
        mst_edges := (e_min, e_max) :: !mst_edges
      end
    done;
    let mst_arr = Array.of_list !mst_edges in
    Array.sort compare mst_arr;
    Printf.printf "GHS MST Weight: %Ld\n" !total_weight;
    Printf.printf "Edges: %d\n" !edges_count;
    for i = 0 to Array.length mst_arr - 1 do
      let (u, v) = mst_arr.(i) in
      Printf.printf "%d %d\n" u v
    done
  with End_of_file -> ()
