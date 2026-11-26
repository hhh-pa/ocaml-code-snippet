(* Uniform random generators (NR 7.1). *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

let vinit = 4101842887655102017L
let mask32 = 0xFFFFFFFFL
let doubc = 5.42101086242752217E-20

let u64_str_u v = Printf.sprintf "%Lu" v
let u64_dbl v = float_of_string (Printf.sprintf "%Lu" v)

type ran = { mutable u : int64; mutable v : int64; mutable w : int64 }

let ran_make j =
  let r = { u = 0L; v = vinit; w = 1L } in
  let step () =
    r.u <- Int64.add (Int64.mul r.u 2862933555777941757L) 7046029254386353087L;
    r.v <- Int64.logxor r.v (Int64.shift_right_logical r.v 17);
    r.v <- Int64.logxor r.v (Int64.shift_left r.v 31);
    r.v <- Int64.logxor r.v (Int64.shift_right_logical r.v 8);
    r.w <-
      Int64.add
        (Int64.mul 4294957665L (Int64.logand r.w mask32))
        (Int64.shift_right_logical r.w 32);
    let x = ref (Int64.logxor r.u (Int64.shift_left r.u 21)) in
    x := Int64.logxor !x (Int64.shift_right_logical !x 35);
    x := Int64.logxor !x (Int64.shift_left !x 4);
    Int64.logxor (Int64.add !x r.v) r.w
  in
  r.u <- Int64.logxor j r.v;
  ignore (step ());
  r.v <- r.u;
  ignore (step ());
  r.w <- r.v;
  ignore (step ());
  (r, step)

type ranq1 = { mutable v : int64 }

let ranq1_make j =
  let r = { v = vinit } in
  let step () =
    r.v <- Int64.logxor r.v (Int64.shift_right_logical r.v 21);
    r.v <- Int64.logxor r.v (Int64.shift_left r.v 35);
    r.v <- Int64.logxor r.v (Int64.shift_right_logical r.v 4);
    Int64.mul r.v 2685821657736338717L
  in
  r.v <- Int64.logxor r.v j;
  r.v <- step ();
  (r, step)

type ranq2 = { mutable v : int64; mutable w : int64 }

let ranq2_make j =
  let r = { v = vinit; w = 1L } in
  let step () =
    r.v <- Int64.logxor r.v (Int64.shift_right_logical r.v 17);
    r.v <- Int64.logxor r.v (Int64.shift_left r.v 31);
    r.v <- Int64.logxor r.v (Int64.shift_right_logical r.v 8);
    r.w <-
      Int64.add
        (Int64.mul 4294957665L (Int64.logand r.w mask32))
        (Int64.shift_right_logical r.w 32);
    Int64.logxor r.v r.w
  in
  r.v <- Int64.logxor r.v j;
  r.w <- step ();
  r.v <- step ();
  (r, step)

let ranhash64 u =
  let v = ref (Int64.add (Int64.mul u 3935559000370003845L) 2691343689449507681L) in
  v := Int64.logxor !v (Int64.shift_right_logical !v 21);
  v := Int64.logxor !v (Int64.shift_left !v 37);
  v := Int64.logxor !v (Int64.shift_right_logical !v 4);
  v := Int64.mul !v 4768777513237032717L;
  v := Int64.logxor !v (Int64.shift_left !v 20);
  v := Int64.logxor !v (Int64.shift_right_logical !v 41);
  v := Int64.logxor !v (Int64.shift_left !v 5);
  !v

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
  let gs i = Int64.of_string toks.(i) and gi i = int_of_string toks.(i) in
  let op = toks.(0) in
  let out = ref [] in
  let emit s = out := s :: !out in
  (if op = "RAN" then (
     let _, step = ran_make (gs 1) in
     for _i = 1 to gi 2 do
       emit (u64_str_u (step ()))
     done)
   else if op = "RAN_D" then (
     let _, step = ran_make (gs 1) in
     for _i = 1 to gi 2 do
       emit (fmt6 (canon (doubc *. u64_dbl (step ()))))
     done)
   else if op = "RAN32" then (
     let _, step = ran_make (gs 1) in
     for _i = 1 to gi 2 do
       emit (u64_str_u (Int64.logand (step ()) mask32))
     done)
   else if op = "RAN8" then (
     let _, step = ran_make (gs 1) in
     let breg = ref 0L and bc = ref 0 in
     for _i = 1 to gi 2 do
       let old = !bc in
       bc := !bc - 1;
       if old <> 0 then (
         breg := Int64.shift_right_logical !breg 8;
         emit (string_of_int (Int64.to_int (Int64.logand !breg 0xFFL))))
       else (
         breg := step ();
         bc := 7;
         emit (string_of_int (Int64.to_int (Int64.logand !breg 0xFFL))))
     done)
   else if op = "RANQ1" then (
     let _, step = ranq1_make (gs 1) in
     for _i = 1 to gi 2 do
       emit (u64_str_u (step ()))
     done)
   else if op = "RANQ1_D" then (
     let _, step = ranq1_make (gs 1) in
     for _i = 1 to gi 2 do
       emit (fmt6 (canon (doubc *. u64_dbl (step ()))))
     done)
   else if op = "RANQ2" then (
     let _, step = ranq2_make (gs 1) in
     for _i = 1 to gi 2 do
       emit (u64_str_u (step ()))
     done)
   else if op = "RANQ2_D" then (
     let _, step = ranq2_make (gs 1) in
     for _i = 1 to gi 2 do
       emit (fmt6 (canon (doubc *. u64_dbl (step ()))))
     done)
   else if op = "HASH64" then emit (u64_str_u (ranhash64 (gs 1)))
   else if op = "HASH32" then
     emit (u64_str_u (Int64.logand (ranhash64 (gs 1)) mask32))
   else if op = "HASH_D" then
     emit (fmt6 (canon (doubc *. u64_dbl (ranhash64 (gs 1)))))
   else if op = "RANFIB_D" || op = "RANFIB_32" then (
     let _, qstep = ranq1_make (gs 1) in
     let dtab = Array.make 55 0.0 in
     for k = 0 to 54 do
       dtab.(k) <- doubc *. u64_dbl (qstep ())
     done;
     let inext = ref 0 and inextp = ref 31 in
     let fstep () =
       inext := !inext + 1;
       if !inext = 55 then inext := 0;
       inextp := !inextp + 1;
       if !inextp = 55 then inextp := 0;
       let dd = ref (dtab.(!inext) -. dtab.(!inextp)) in
       if !dd < 0.0 then dd := !dd +. 1.0;
       dtab.(!inext) <- !dd;
       !dd
     in
     for _i = 1 to gi 2 do
       if op = "RANFIB_D" then emit (fmt6 (canon (fstep ())))
       else emit (string_of_int (int_of_float (fstep () *. 4294967295.0)))
     done));
  print_endline (String.concat " " (List.rev !out))
