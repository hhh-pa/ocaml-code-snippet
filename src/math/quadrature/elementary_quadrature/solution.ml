(* Elementary quadrature (NR 4.2). Port of the C++ reference. *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

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
    let v = toks.(!pos) in
    incr pos; v
  in
  let op = next_tok () in
  let fun_ = next_tok () in
  let p =
    if fun_ = "POLY" then (
      let d = int_of_string (next_tok ()) in
      Array.init (d + 1) (fun _ -> float_of_string (next_tok ())))
    else if fun_ = "POW" then Array.init 4 (fun _ -> float_of_string (next_tok ()))
    else Array.init 3 (fun _ -> float_of_string (next_tok ()))
  in
  let a = float_of_string (next_tok ()) in
  let b = float_of_string (next_tok ()) in
  let f t =
    if fun_ = "POLY" then (
      let s = ref 0.0 in
      for i = Array.length p - 1 downto 0 do
        s := !s *. t +. p.(i)
      done;
      !s)
    else if fun_ = "EXP" then p.(0) *. exp (p.(1) *. t) +. p.(2)
    else if fun_ = "TRIG" then p.(0) *. sin (p.(1) *. t +. p.(2))
    else if fun_ = "LOG" then p.(0) *. log (p.(1) *. t +. p.(2))
    else p.(0) *. ((p.(1) *. t +. p.(2)) ** p.(3))
  in
  let n = ref 0 and s = ref 0.0 in
  let next () =
    incr n;
    if !n = 1 then (
      s := 0.5 *. (b -. a) *. (f a +. f b);
      !s)
    else (
      let it = ref 1 in
      for _j = 1 to !n - 2 do
        it := !it * 2
      done;
      let tnm = float !it in
      let del = (b -. a) /. tnm in
      let x = ref (a +. 0.5 *. del) and sum = ref 0.0 in
      for _j = 0 to !it - 1 do
        sum := !sum +. f !x;
        x := !x +. del
      done;
      s := 0.5 *. (!s +. (b -. a) *. !sum /. tnm);
      !s)
  in
  let eps = 1.0e-10 and jmax = 20 in
  let ans = ref 0.0 in
  (if op = "TRAPZD" then (
     let l = int_of_string (next_tok ()) in
     for _j = 0 to l - 1 do
       ans := next ()
     done)
   else if op = "QTRAP" then (
     let olds = ref 0.0 in
     (try
        for j = 0 to jmax - 1 do
          ans := next ();
          if j > 5
             && (abs_float (!ans -. !olds) < eps *. abs_float !olds
                || (!ans = 0.0 && !olds = 0.0))
          then raise Exit;
          olds := !ans
        done
      with Exit -> ()))
   else (
     let ost = ref 0.0 and os = ref 0.0 in
     (try
        for j = 0 to jmax - 1 do
          let st = next () in
          ans := (4.0 *. st -. !ost) /. 3.0;
          if j > 5
             && (abs_float (!ans -. !os) < eps *. abs_float !os
                || (!ans = 0.0 && !os = 0.0))
          then raise Exit;
          os := !ans;
          ost := st
        done
      with Exit -> ())));
  print_endline (fmt6 (canon !ans))
