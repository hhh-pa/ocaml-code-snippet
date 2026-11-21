(* Chebyshev approximation (NR 5.8-5.11). Port of the C++ reference. *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v
let fmtv v = fmt6 (canon v)
let pi = 3.141592653589793

let chebev x c =
  let m = Array.length c in
  let d = ref 0.0 and dd = ref 0.0 in
  for j = m - 1 downto 1 do
    let sv = !d in
    d := 2.0 *. x *. !d -. !dd +. c.(j);
    dd := sv
  done;
  x *. !d -. !dd +. 0.5 *. c.(0)

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
  let op = next () in
  if op = "FIT" then (
    let fun_ = next () in
    let p =
      if fun_ = "POLY" then (
        let d = int_of_string (next ()) in
        Array.init (d + 1) (fun _ -> float_of_string (next ())))
      else Array.init 3 (fun _ -> float_of_string (next ()))
    in
    let a = float_of_string (next ()) in
    let b = float_of_string (next ()) in
    let n = int_of_string (next ()) in
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
    let c = Array.make n 0.0 in
    for j = 0 to n - 1 do
      let s = ref 0.0 in
      for k = 0 to n - 1 do
        let y = cos (pi *. (float k +. 0.5) /. float n) in
        let t = 0.5 *. ((b -. a) *. y +. (a +. b)) in
        s := !s +. f t *. cos (pi *. float j *. (float k +. 0.5) /. float n)
      done;
      c.(j) <- 2.0 *. !s /. float n
    done;
    for j = 0 to n - 1 do
      if j > 0 then print_string " ";
      print_string (fmtv c.(j))
    done;
    print_newline ())
  else if op = "EVAL" then (
    let n = int_of_string (next ()) in
    let x = float_of_string (next ()) in
    let c = Array.init n (fun _ -> float_of_string (next ())) in
    print_endline (fmtv (chebev x c)))
  else if op = "DERIV" then (
    let n = int_of_string (next ()) in
    let a = float_of_string (next ()) in
    let b = float_of_string (next ()) in
    let c = Array.init n (fun _ -> float_of_string (next ())) in
    let cd = Array.make n 0.0 in
    cd.(n - 1) <- 0.0;
    if n >= 2 then cd.(n - 2) <- 2.0 *. float (n - 1) *. c.(n - 1);
    for j = n - 2 downto 1 do
      cd.(j - 1) <- cd.(j + 1) +. 2.0 *. float j *. c.(j)
    done;
    let con = 2.0 /. (b -. a) in
    for j = 0 to n - 1 do
      if j > 0 then print_string " ";
      print_string (fmtv (cd.(j) *. con))
    done;
    print_newline ())
  else if op = "INTEG" then (
    let n = int_of_string (next ()) in
    let a = float_of_string (next ()) in
    let b = float_of_string (next ()) in
    let c = Array.init n (fun _ -> float_of_string (next ())) in
    let ci = Array.make n 0.0 in
    let con = 0.25 *. (b -. a) in
    let sum = ref 0.0 and fac = ref 1.0 in
    for j = 1 to n - 2 do
      ci.(j) <- con *. (c.(j - 1) -. c.(j + 1)) /. float j;
      sum := !sum +. !fac *. ci.(j);
      fac := -. !fac
    done;
    ci.(n - 1) <- con *. c.(n - 2) /. float (n - 1);
    sum := !sum +. !fac *. ci.(n - 1);
    ci.(0) <- 2.0 *. !sum;
    for j = 0 to n - 1 do
      if j > 0 then print_string " ";
      print_string (fmtv ci.(j))
    done;
    print_newline ())
  else (
    let n = int_of_string (next ()) in
    let c = Array.init n (fun _ -> float_of_string (next ())) in
    let d = Array.make n 0.0 and dd = Array.make n 0.0 in
    d.(0) <- c.(n - 1);
    for j = n - 2 downto 1 do
      for k = n - j downto 1 do
        let sv = d.(k) in
        d.(k) <- 2.0 *. d.(k - 1) -. dd.(k);
        dd.(k) <- sv
      done;
      let sv = d.(0) in
      d.(0) <- -.dd.(0) +. c.(j);
      dd.(0) <- sv
    done;
    for j = n - 1 downto 1 do
      d.(j) <- d.(j - 1) -. dd.(j)
    done;
    d.(0) <- -.dd.(0) +. 0.5 *. c.(0);
    for j = 0 to n - 1 do
      if j > 0 then print_string " ";
      print_string (fmtv d.(j))
    done;
    print_newline ())
