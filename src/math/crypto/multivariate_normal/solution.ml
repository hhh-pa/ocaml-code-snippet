(* Multivariate normal deviates (NR 7.4). *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

type ran = { mutable u : int64; mutable v : int64; mutable w : int64 }

let doubc = 5.42101086242752217E-20
let u64_dbl v = float_of_string (Printf.sprintf "%Lu" v)

let ran_step r =
  r.u <- Int64.add (Int64.mul r.u 2862933555777941757L) 7046029254386353087L;
  r.v <- Int64.logxor r.v (Int64.shift_right_logical r.v 17);
  r.v <- Int64.logxor r.v (Int64.shift_left r.v 31);
  r.v <- Int64.logxor r.v (Int64.shift_right_logical r.v 8);
  r.w <-
    Int64.add
      (Int64.mul 4294957665L (Int64.logand r.w 0xFFFFFFFFL))
      (Int64.shift_right_logical r.w 32);
  let x = ref (Int64.logxor r.u (Int64.shift_left r.u 21)) in
  x := Int64.logxor !x (Int64.shift_right_logical !x 35);
  x := Int64.logxor !x (Int64.shift_left !x 4);
  Int64.logxor (Int64.add !x r.v) r.w

let ran_make j =
  let r = { u = 0L; v = 4101842887655102017L; w = 1L } in
  r.u <- Int64.logxor j r.v;
  ignore (ran_step r);
  r.v <- r.u;
  ignore (ran_step r);
  r.w <- r.v;
  ignore (ran_step r);
  r

let ran_doub r = doubc *. u64_dbl (ran_step r)

let () =
  let toks =
    String.split_on_char ' '
      (String.concat " "
         (let rec rd acc =
            try rd (input_line stdin :: acc) with End_of_file -> List.rev acc
          in
          rd []))
  in
  let toks = Array.of_list (List.filter (fun s -> s <> "") toks) in
  if Array.length toks = 0 then exit 0;
  let pos = ref 0 in
  let next_tok () =
    let t = toks.(!pos) in
    pos := !pos + 1;
    t
  in
  let mm = int_of_string (next_tok ()) in
  let mean = Array.init mm (fun _ -> float_of_string (next_tok ())) in
  let a = Array.init mm (fun _ -> Array.init mm (fun _ -> float_of_string (next_tok ()))) in
  let seed = Int64.of_string (next_tok ()) and n = int_of_string (next_tok ()) in
  let l = Array.make_matrix mm mm 0.0 in
  for i = 0 to mm - 1 do
    for j = 0 to i do
      let s = ref a.(i).(j) in
      for k = 0 to j - 1 do
        s := !s -. l.(i).(k) *. l.(j).(k)
      done;
      l.(i).(j) <- (if i = j then sqrt !s else !s /. l.(j).(j))
    done
  done;
  let r = ran_make seed in
  let spt = Array.make mm 0.0 and pt = Array.make mm 0.0 in
  for _d = 1 to n do
    for i = 0 to mm - 1 do
      let cont = ref true in
      while !cont do
        let u = ran_doub r and v = 1.7156 *. (ran_doub r -. 0.5) in
        let x = u -. 0.449871 and y = abs_float v +. 0.386595 in
        let q = x *. x +. y *. (0.19600 *. y -. 0.25472 *. x) in
        if not (q > 0.27597 && (q > 0.27846 || v *. v > -4.0 *. log u *. u *. u)) then (
          spt.(i) <- v /. u;
          cont := false)
      done
    done;
    for i = 0 to mm - 1 do
      let s = ref 0.0 in
      for j = 0 to i do
        s := !s +. l.(i).(j) *. spt.(j)
      done;
      pt.(i) <- !s +. mean.(i)
    done;
    print_endline
      (String.concat " " (Array.to_list (Array.map (fun v -> fmt6 (canon v)) pt)))
  done
