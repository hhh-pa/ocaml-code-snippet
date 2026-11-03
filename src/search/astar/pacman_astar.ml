module NodeSet = Set.Make(struct
  type t = int * int * int (* cost, r, c *)
  let compare (c1, r1, c1_) (c2, r2, c2_) =
    if c1 <> c2 then compare c1 c2
    else if r1 <> r2 then compare r1 r2
    else compare c1_ c2_
end)

let manhattan r1 c1 r2 c2 =
  abs (r1 - r2) + abs (c1 - c2)

let dirs = [(-1, 0); (0, -1); (0, 1); (1, 0)]

let () =
  try
    let pr = Scanf.scanf " %d" (fun x -> x) in
    let pc = Scanf.scanf " %d" (fun x -> x) in
    let fr = Scanf.scanf " %d" (fun x -> x) in
    let fc = Scanf.scanf " %d" (fun x -> x) in
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let grid = Array.make n "" in
    for i = 0 to n - 1 do
      grid.(i) <- Scanf.scanf " %s" (fun s -> s)
    done;

    let dist = Array.make_matrix n m (-1) in
    let parent = Array.make_matrix n m (-1, -1) in
    let node_cost = Array.make_matrix n m (-1) in
    let q = ref NodeSet.empty in

    let init_cost = manhattan pr pc fr fc in
    q := NodeSet.add (init_cost, pr, pc) !q;
    dist.(pr).(pc) <- 0;
    node_cost.(pr).(pc) <- init_cost;

    while not (NodeSet.is_empty !q) do
      let (cost, r, c) = NodeSet.min_elt !q in
      q := NodeSet.remove (cost, r, c) !q;
      List.iter (fun (dr, dc) ->
        let nr = r + dr in
        let nc = c + dc in
        if nr >= 0 && nr < n && nc >= 0 && nc < m && grid.(nr).[nc] <> '%' then begin
          let new_dist = dist.(r).(c) + 1 in
          let new_cost = dist.(r).(c) + manhattan nr nc fr fc in
          if dist.(nr).(nc) = -1 || dist.(nr).(nc) > new_dist then begin
            if dist.(nr).(nc) <> -1 then begin
              let old_c = node_cost.(nr).(nc) in
              q := NodeSet.remove (old_c, nr, nc) !q
            end;
            dist.(nr).(nc) <- new_dist;
            parent.(nr).(nc) <- (r, c);
            node_cost.(nr).(nc) <- new_cost;
            q := NodeSet.add (new_cost, nr, nc) !q
          end
        end
      ) dirs
    done;

    if dist.(fr).(fc) <> -1 then begin
      let path = ref [(fr, fc)] in
      let step = ref (fr, fc) in
      while !step <> (pr, pc) do
        let (sr, sc) = !step in
        step := parent.(sr).(sc);
        path := !step :: !path
      done;
      let path_list = !path in
      Printf.printf "%d\n" (List.length path_list - 1);
      List.iter (fun (r, c) -> Printf.printf "%d %d\n" r c) path_list
    end
  with End_of_file -> ()
