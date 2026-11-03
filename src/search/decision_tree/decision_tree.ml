type node =
  | Leaf of int * int (* depth, label *)
  | Internal of int * int * node * node (* depth, feat, left, right *)

let solve_case () =
  let n = Scanf.scanf " %d" (fun x -> x) in
  let f_cnt = Scanf.scanf " %d" (fun x -> x) in
  let d = Scanf.scanf " %d" (fun x -> x) in
  let x_mat = Array.make_matrix n f_cnt 0 in
  let y_arr = Array.make n 0 in
  for i = 0 to n - 1 do
    for f = 0 to f_cnt - 1 do
      x_mat.(i).(f) <- Scanf.scanf " %d" (fun x -> x)
    done;
    y_arr.(i) <- Scanf.scanf " %d" (fun x -> x)
  done;

  let entropy idx =
    match idx with
    | [] -> 0.0
    | _ ->
        let c = ref 0 in
        List.iter (fun i -> if y_arr.(i) = 1 then incr c) idx;
        let len = float_of_int (List.length idx) in
        let p = float_of_int !c /. len in
        if p = 0.0 || p = 1.0 then 0.0
        else -. (p *. log p +. (1.0 -. p) *. log (1.0 -. p))
  in

  let rec build idx used depth pmaj =
    let c0 = ref 0 in
    let c1 = ref 0 in
    List.iter (fun i -> if y_arr.(i) = 1 then incr c1 else incr c0) idx;
    if idx = [] then Leaf (depth, pmaj)
    else if !c0 = 0 || !c1 = 0 || depth >= d || used = (1 lsl f_cnt) - 1 then
      Leaf (depth, if !c1 >= !c0 then 1 else 0)
    else begin
      let base = entropy idx in
      let bf = ref 0 in
      let bg = ref (-1.0) in
      let n_idx = float_of_int (List.length idx) in
      for f = 0 to f_cnt - 1 do
        if (used land (1 lsl f)) = 0 then begin
          let l_acc = ref [] in
          let r_acc = ref [] in
          List.iter (fun i ->
            if x_mat.(i).(f) = 0 then l_acc := i :: !l_acc
            else r_acc := i :: !r_acc
          ) idx;
          let l_list = !l_acc in
          let r_list = !r_acc in
          let len_l = float_of_int (List.length l_list) in
          let len_r = float_of_int (List.length r_list) in
          let g = base -. (len_l /. n_idx) *. entropy l_list -. (len_r /. n_idx) *. entropy r_list in
          if g > !bg +. 1e-12 then begin
            bg := g;
            bf := f
          end
        end
      done;
      let maj = if !c1 >= !c0 then 1 else 0 in
      let l_acc = ref [] in
      let r_acc = ref [] in
      List.iter (fun i ->
        if x_mat.(i).(!bf) = 0 then l_acc := i :: !l_acc
        else r_acc := i :: !r_acc
      ) idx;
      let l_child = build (List.rev !l_acc) (used lor (1 lsl !bf)) (depth + 1) maj in
      let r_child = build (List.rev !r_acc) (used lor (1 lsl !bf)) (depth + 1) maj in
      Internal (depth, !bf, l_child, r_child)
    end
  in

  let rec predict nd i =
    match nd with
    | Leaf (_, lbl) -> lbl
    | Internal (_, feat, left, right) ->
        if x_mat.(i).(feat) = 0 then predict left i
        else predict right i
  in

  let rec maxdepth = function
    | Leaf (dep, _) -> dep
    | Internal (_, _, left, right) -> max (maxdepth left) (maxdepth right)
  in

  let all_indices = List.init n (fun i -> i) in
  let root = build all_indices 0 0 1 in
  Printf.printf "DT %d\n" (maxdepth root);
  for i = 0 to n - 1 do
    if i > 0 then Printf.printf " ";
    Printf.printf "%d" (predict root i)
  done;
  Printf.printf "\n"

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
