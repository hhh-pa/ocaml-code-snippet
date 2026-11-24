(* Radial basis function interpolation (NR 3.7). *)

let fmt v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let () =
  let toks = String.split_on_char ' ' (String.concat " " (let rec rd acc =
    try rd (input_line stdin :: acc) with End_of_file -> List.rev acc in rd [])) in
  let toks = List.filter (fun s -> s <> "") toks in
  match toks with
  | [] -> ()
  | op :: rest ->
    let r0 = float_of_string (List.nth rest 0) in
    let normflag = int_of_string (List.nth rest 1) <> 0 in
    let n = int_of_string (List.nth rest 2) in
    let dim = int_of_string (List.nth rest 3) in
    let nums = List.map float_of_string (List.tl (List.tl (List.tl (List.tl rest)))) in
    let arr = Array.of_list nums in
    let pts = Array.init n (fun i -> Array.init dim (fun j -> arr.(i * dim + j))) in
    let vals = Array.init n (fun i -> arr.(n * dim + i)) in
    let q = int_of_float arr.(n * dim + n) in
    let qs = Array.init q (fun i ->
      Array.init dim (fun j -> arr.(n * dim + n + 1 + i * dim + j))) in
    let phi r =
      if op = "MULTIQUADRIC" then sqrt (r *. r +. r0 *. r0)
      else if op = "THINPLATE" then (if r <= 0.0 then 0.0 else r *. r *. log (r /. r0))
      else if op = "GAUSS" then (let t = r /. r0 in exp (-0.5 *. t *. t))
      else 1.0 /. sqrt (r *. r +. r0 *. r0) in
    let rad a b =
      let s = ref 0.0 in
      for i = 0 to dim - 1 do s := !s +. (a.(i) -. b.(i)) *. (a.(i) -. b.(i)) done;
      sqrt !s in
    let phim = Array.make_matrix n n 0.0 in
    let rhs = Array.make n 0.0 in
    for i = 0 to n - 1 do
      let sum = ref 0.0 in
      for j = 0 to n - 1 do
        let v = phi (rad pts.(i) pts.(j)) in
        phim.(i).(j) <- v;
        sum := !sum +. v
      done;
      rhs.(i) <- (if normflag then !sum *. vals.(i) else vals.(i))
    done;
    let p = Array.init n (fun i -> i) in
    for k = 0 to n - 1 do
      let piv = ref k in
      for i = k + 1 to n - 1 do
        if abs_float phim.(i).(k) > abs_float phim.(!piv).(k) then piv := i
      done;
      if !piv <> k then begin
        let t = phim.(k) in phim.(k) <- phim.(!piv); phim.(!piv) <- t;
        let ti = p.(k) in p.(k) <- p.(!piv); p.(!piv) <- ti
      end;
      for i = k + 1 to n - 1 do
        phim.(i).(k) <- phim.(i).(k) /. phim.(k).(k);
        for j = k + 1 to n - 1 do
          phim.(i).(j) <- phim.(i).(j) -. phim.(i).(k) *. phim.(k).(j)
        done
      done
    done;
    let y = Array.make n 0.0 in
    for i = 0 to n - 1 do
      let s = ref rhs.(p.(i)) in
      for j = 0 to i - 1 do s := !s -. phim.(i).(j) *. y.(j) done;
      y.(i) <- !s
    done;
    let w = Array.make n 0.0 in
    for i = n - 1 downto 0 do
      let s = ref y.(i) in
      for j = i + 1 to n - 1 do s := !s -. phim.(i).(j) *. w.(j) done;
      w.(i) <- !s /. phim.(i).(i)
    done;
    for qq = 0 to q - 1 do
      let sumw = ref 0.0 and sum = ref 0.0 in
      for i = 0 to n - 1 do
        let f = phi (rad qs.(qq) pts.(i)) in
        sumw := !sumw +. w.(i) *. f;
        sum := !sum +. f
      done;
      print_endline (fmt (if normflag then !sumw /. !sum else !sumw))
    done
