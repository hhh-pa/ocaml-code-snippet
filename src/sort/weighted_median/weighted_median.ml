let weighted_median a =
  let n = Array.length a in
  let l = ref 0 in
  let r = ref (n - 1) in
  let w_left = ref 0.0 in
  let res = ref 0L in
  let found = ref false in
  while !l < !r && not !found do
    let len = !r - !l + 1 in
    let sub = Array.sub a !l len in
    Array.sort (fun (v1, _) (v2, _) -> compare v1 v2) sub;
    let pivot_val = fst sub.(len / 2) in
    let less = ref [] in
    let eq = ref [] in
    let greater = ref [] in
    for i = !l to !r do
      let (v, w) = a.(i) in
      if v < pivot_val then less := (v, w) :: !less
      else if v = pivot_val then eq := (v, w) :: !eq
      else greater := (v, w) :: !greater
    done;
    let less_list = List.rev !less in
    let eq_list = List.rev !eq in
    let greater_list = List.rev !greater in
    let w_less = List.fold_left (fun acc (_, w) -> acc +. w) 0.0 less_list in
    let w_eq = List.fold_left (fun acc (_, w) -> acc +. w) 0.0 eq_list in
    let idx = ref !l in
    List.iter (fun pr -> a.(!idx) <- pr; incr idx) less_list;
    let eq_start = !idx in
    List.iter (fun pr -> a.(!idx) <- pr; incr idx) eq_list;
    List.iter (fun pr -> a.(!idx) <- pr; incr idx) greater_list;
    let eq_end = eq_start + List.length eq_list - 1 in

    if !w_left +. w_less < 0.5 && !w_left +. w_less +. w_eq >= 0.5 then begin
      res := pivot_val;
      found := true
    end else if !w_left +. w_less >= 0.5 then
      r := eq_start - 1
    else begin
      w_left := !w_left +. w_less +. w_eq;
      l := eq_end + 1
    end
  done;
  if !found then !res else fst a.(!l)

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    if n = 0 then
      Printf.printf "0\n"
    else begin
      let vals = Array.init n (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
      let wts = Array.init n (fun _ -> Scanf.scanf " %f" (fun x -> x)) in
      let a = Array.init n (fun i -> (vals.(i), wts.(i))) in
      let ans = weighted_median a in
      Printf.printf "%Ld\n" ans
    end
  with End_of_file -> ()
