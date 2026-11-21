(* Romberg integration (NR 4.3). Port of the C++ reference. *)

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
  let eps = 1.0e-10 and jmax = 20 and k = 5 in
  let ss = Array.make jmax 0.0 in
  let h = Array.make (jmax + 1) 0.0 in
  h.(0) <- 1.0;
  let ans = ref 0.0 in
  (try
     for j = 1 to jmax do
       ss.(j - 1) <- next ();
       if j >= k then (
         let c = Array.make k 0.0 and d = Array.make k 0.0 in
         let dif = ref (abs_float (0.0 -. h.(j - k))) and ns = ref 0 in
         for i = 0 to k - 1 do
           let dift = abs_float (0.0 -. h.(j - k + i)) in
           if dift < !dif then (
             ns := i;
             dif := dift);
           c.(i) <- ss.(j - k + i);
           d.(i) <- ss.(j - k + i)
         done;
         let y = ref ss.(j - k + !ns) and dy = ref 0.0 in
         ns := !ns - 1;
         for mm = 1 to k - 1 do
           for i = 0 to k - mm - 1 do
             let ho = h.(j - k + i) in
             let hp = h.(j - k + i + mm) in
             let den = (c.(i + 1) -. d.(i)) /. (ho -. hp) in
             d.(i) <- hp *. den;
             c.(i) <- ho *. den
           done;
           (if 2 * (!ns + 1) < k - mm then dy := c.(!ns + 1)
            else (
              dy := d.(!ns);
              ns := !ns - 1));
           y := !y +. !dy
         done;
         if abs_float !dy <= eps *. abs_float !y then (
           ans := !y;
           raise Exit);
         ans := !y);
       h.(j) <- 0.25 *. h.(j - 1)
     done
   with Exit -> ());
  print_endline (fmt6 (canon !ans))
