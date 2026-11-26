(* Numerical derivative (NR 5.7, Ridders' method). *)

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
  let next () =
    let v = toks.(!pos) in
    incr pos; v
  in
  let fun_ = next () in
  let p =
    if fun_ = "POLY" then (
      let d = int_of_string (next ()) in
      Array.init (d + 1) (fun _ -> float_of_string (next ())))
    else Array.init 3 (fun _ -> float_of_string (next ()))
  in
  let x = float_of_string (next ()) in
  let h = ref (float_of_string (next ())) in
  let f t =
    if fun_ = "POLY" then (
      let s = ref 0.0 in
      for i = Array.length p - 1 downto 0 do
        s := !s *. t +. p.(i)
      done;
      !s)
    else if fun_ = "EXP" then p.(0) *. exp (p.(1) *. t) +. p.(2)
    else if fun_ = "TRIG" then p.(0) *. sin (p.(1) *. t +. p.(2))
    else p.(0) *. log (p.(1) *. t +. p.(2))
  in
  let con = 1.4 in
  let con2 = con *. con in
  let big = 1.0e30 in
  let ntab = 10 in
  let a = Array.make_matrix ntab ntab 0.0 in
  a.(0).(0) <- (f (x +. !h) -. f (x -. !h)) /. (2.0 *. !h);
  let err = ref big and ans = ref a.(0).(0) in
  for i = 1 to ntab - 1 do
    h := !h /. con;
    a.(0).(i) <- (f (x +. !h) -. f (x -. !h)) /. (2.0 *. !h);
    let fac = ref con2 in
    for j = 1 to i do
      a.(j).(i) <- (a.(j - 1).(i) *. !fac -. a.(j - 1).(i - 1)) /. (!fac -. 1.0);
      fac := !fac *. con2;
      let errt = ref (abs_float (a.(j).(i) -. a.(j - 1).(i))) in
      let e2 = abs_float (a.(j).(i) -. a.(j - 1).(i - 1)) in
      if e2 > !errt then errt := e2;
      if !errt <= !err then (
        err := !errt;
        ans := a.(j).(i))
    done
  done;
  print_string (fmt6 (canon !ans));
  print_string " ";
  print_string (fmt6 (canon !err));
  print_newline ()
