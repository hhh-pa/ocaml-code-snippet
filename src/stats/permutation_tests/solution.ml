(* Permutation tests (Think Stats 2e ch 9). *)

let read_all () =
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_char buf (input_char stdin)
     done
   with End_of_file -> ());
  Buffer.contents buf

let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let split_ws s =
  let toks = ref [] in
  let cur = Buffer.create 16 in
  let flush () =
    if Buffer.length cur > 0 then begin
      toks := Buffer.contents cur :: !toks;
      Buffer.clear cur
    end
  in
  String.iter
    (fun c -> if is_space c then flush () else Buffer.add_char cur c)
    s;
  flush ();
  List.rev !toks

let modulus = 1 lsl 31

let draw rng u =
  rng := ((1103515245 * !rng) + 12345) mod modulus;
  !rng mod u

let shuffle_fy pool rng =
  let a = Array.copy pool in
  for i = Array.length a - 1 downto 1 do
    let j = draw rng (i + 1) in
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t
  done;
  a

let pearson xs ys =
  let n = Array.length xs in
  let sx = Array.fold_left ( +. ) 0.0 xs in
  let sy = Array.fold_left ( +. ) 0.0 ys in
  let mx = sx /. float_of_int n in
  let my = sy /. float_of_int n in
  let c = ref 0.0 in
  let vx = ref 0.0 in
  let vy = ref 0.0 in
  for i = 0 to n - 1 do
    let dx = xs.(i) -. mx in
    let dy = ys.(i) -. my in
    c := !c +. (dx *. dy);
    vx := !vx +. (dx *. dx);
    vy := !vy +. (dy *. dy)
  done;
  if !vx = 0.0 || !vy = 0.0 then 0.0 else !c /. sqrt (!vx *. !vy)

let isum a =
  let s = ref 0 in
  Array.iter (fun x -> s := !s + x) a;
  !s

let run_diff g1 g2 k seed =
  let n1 = Array.length g1 in
  let n2 = Array.length g2 in
  let rng = ref seed in
  let s1 = isum g1 in
  let s2 = isum g2 in
  let obs =
    (float_of_int s1 /. float_of_int n1) -. (float_of_int s2 /. float_of_int n2)
  in
  let nobs = abs ((s1 * n2) - (s2 * n1)) in
  let pool = Array.append g1 g2 in
  let cnt = ref 0 in
  for _ = 1 to k do
    let a = shuffle_fy pool rng in
    let t1 = ref 0 in
    let t2 = ref 0 in
    for i = 0 to n1 - 1 do
      t1 := !t1 + a.(i)
    done;
    for i = n1 to Array.length a - 1 do
      t2 := !t2 + a.(i)
    done;
    if abs ((!t1 * n2) - (!t2 * n1)) >= nobs then incr cnt
  done;
  (obs, !cnt)

let run_corr xs ys k seed =
  let rng = ref seed in
  let ysd = Array.map float_of_int ys in
  let obs = pearson xs ysd in
  let arobs = abs_float obs in
  let cnt = ref 0 in
  for _ = 1 to k do
    let a = shuffle_fy ys rng in
    let ad = Array.map float_of_int a in
    if abs_float (pearson xs ad) >= arobs then incr cnt
  done;
  (obs, !cnt)

let count_ones a lo hi =
  let c = ref 0 in
  for i = lo to hi - 1 do
    if a.(i) = 1 then incr c
  done;
  !c

let run_prop g1 g2 k seed =
  let n1 = Array.length g1 in
  let n2 = Array.length g2 in
  let rng = ref seed in
  let c1 = count_ones g1 0 n1 in
  let c2 = count_ones g2 0 n2 in
  let obs =
    (float_of_int c1 /. float_of_int n1) -. (float_of_int c2 /. float_of_int n2)
  in
  let nobs = abs ((c1 * n2) - (c2 * n1)) in
  let pool = Array.append g1 g2 in
  let cnt = ref 0 in
  for _ = 1 to k do
    let a = shuffle_fy pool rng in
    let d1 = count_ones a 0 n1 in
    let d2 = count_ones a n1 (Array.length a) in
    if abs ((d1 * n2) - (d2 * n1)) >= nobs then incr cnt
  done;
  (obs, !cnt)

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let arr = Array.of_list toks in
      let mode = arr.(0) in
      let obs, cnt, k =
        if mode = "DIFF" then
          let n1 = int_of_string arr.(1) in
          let g1 = Array.init n1 (fun i -> int_of_string arr.(2 + i)) in
          let n2 = int_of_string arr.(2 + n1) in
          let g2 = Array.init n2 (fun i -> int_of_string arr.(3 + n1 + i)) in
          let k = int_of_string arr.(3 + n1 + n2) in
          let seed = int_of_string arr.(4 + n1 + n2) in
          let obs, cnt = run_diff g1 g2 k seed in
          (obs, cnt, k)
        else if mode = "CORR" then
          let n = int_of_string arr.(1) in
          let xs =
            Array.init n (fun i ->
                float_of_int (int_of_string arr.(2 + (2 * i))))
          in
          let ys = Array.init n (fun i -> int_of_string arr.(3 + (2 * i))) in
          let k = int_of_string arr.(2 + (2 * n)) in
          let seed = int_of_string arr.(3 + (2 * n)) in
          let obs, cnt = run_corr xs ys k seed in
          (obs, cnt, k)
        else
          let n1 = int_of_string arr.(1) in
          let g1 = Array.init n1 (fun i -> int_of_string arr.(2 + i)) in
          let n2 = int_of_string arr.(2 + n1) in
          let g2 = Array.init n2 (fun i -> int_of_string arr.(3 + n1 + i)) in
          let k = int_of_string arr.(3 + n1 + n2) in
          let seed = int_of_string arr.(4 + n1 + n2) in
          let obs, cnt = run_prop g1 g2 k seed in
          (obs, cnt, k)
      in
      Printf.printf "OBS %.6f\nP %.6f" obs (float_of_int cnt /. float_of_int k)
