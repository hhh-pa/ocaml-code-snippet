type dsu = {
  parent : int array;
  rank : int array;
  mutable components : int;
}

let create_dsu n =
  { parent = Array.init n (fun i -> i);
    rank = Array.make n 0;
    components = n }

let rec find dsu i =
  if dsu.parent.(i) = i then i
  else begin
    let root = find dsu dsu.parent.(i) in
    dsu.parent.(i) <- root;
    root
  end

let unite dsu i j =
  let root_i = find dsu i in
  let root_j = find dsu j in
  if root_i = root_j then false
  else begin
    let r_i, r_j =
      if dsu.rank.(root_i) < dsu.rank.(root_j) then (root_j, root_i)
      else (root_i, root_j)
    in
    dsu.parent.(r_j) <- r_i;
    if dsu.rank.(r_i) = dsu.rank.(r_j) then
      dsu.rank.(r_i) <- dsu.rank.(r_i) + 1;
    dsu.components <- dsu.components - 1;
    true
  end

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let edges = Array.init m (fun _ ->
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      let w = Scanf.scanf " %Ld" (fun x -> x) in
      (u, v, w)
    ) in
    let dsu = create_dsu n in
    let mst_weight = ref 0L in
    let edges_added = ref 0 in
    let continue_loop = ref true in
    while dsu.components > 1 && !continue_loop do
      let cheapest = Array.make n (-1) in
      for idx = 0 to m - 1 do
        let (u, v, w) = edges.(idx) in
        let root_u = find dsu u in
        let root_v = find dsu v in
        if root_u <> root_v then begin
          let update root =
            if cheapest.(root) = -1 then
              cheapest.(root) <- idx
            else
              let (_, _, old_w) = edges.(cheapest.(root)) in
              if w < old_w then cheapest.(root) <- idx
          in
          update root_u;
          update root_v
        end
      done;
      let progress = ref false in
      for i = 0 to n - 1 do
        let edge_idx = cheapest.(i) in
        if edge_idx <> -1 then begin
          let (u, v, w) = edges.(edge_idx) in
          if unite dsu u v then begin
            mst_weight := Int64.add !mst_weight w;
            incr edges_added;
            progress := true
          end
        end
      done;
      if not !progress then continue_loop := false
    done;
    if !edges_added = n - 1 then
      Printf.printf "%Ld\n" !mst_weight
    else
      Printf.printf "DISCONNECTED\n"
  with End_of_file -> ()
