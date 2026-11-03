type dsu = {
  parent : int array;
  rank : int array;
}

let create_dsu n =
  { parent = Array.init n (fun i -> i);
    rank = Array.make n 0 }

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
    Array.sort (fun (u1, v1, w1) (u2, v2, w2) ->
      let c = compare (w1 : int64) w2 in
      if c <> 0 then c
      else
        let c2 = compare (u1 : int) u2 in
        if c2 <> 0 then c2
        else compare (v1 : int) v2
    ) edges;
    let dsu = create_dsu n in
    let mw = ref 0L in
    let bn = ref 0L in
    let cnt = ref 0 in
    Array.iter (fun (u, v, w) ->
      if unite dsu u v then begin
        mw := Int64.add !mw w;
        if w > !bn then bn := w;
        incr cnt
      end
    ) edges;
    if n > 0 && !cnt <> n - 1 then
      Printf.printf "DISCONNECTED\n"
    else begin
      Printf.printf "%Ld\n" !bn;
      Printf.printf "%Ld\n" !mw
    end
  with End_of_file -> ()
