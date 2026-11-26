(* Pade approximants (NR 5.12). *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v
let fmtv v = fmt6 (canon v)

let lusolve lu p rhs n =
  let y = Array.make n 0.0 and x = Array.make n 0.0 in
  for i = 0 to n - 1 do
    let s = ref rhs.(p.(i)) in
    for j = 0 to i - 1 do
      s := !s -. lu.(i).(j) *. y.(j)
    done;
    y.(i) <- !s
  done;
  for i = n - 1 downto 0 do
    let s = ref y.(i) in
    for j = i + 1 to n - 1 do
      s := !s -. lu.(i).(j) *. x.(j)
    done;
    x.(i) <- !s /. lu.(i).(i)
  done;
  x

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
  let n = int_of_string (next ()) in
  let c = Array.init (2 * n + 1) (fun _ -> float_of_string (next ())) in
  let a0 = Array.make_matrix n n 0.0 in
  let rhs = Array.make n 0.0 in
  for k = 1 to n do
    for m = 1 to n do
      a0.(k - 1).(m - 1) <- c.(n - m + k)
    done;
    rhs.(k - 1) <- -.c.(n + k)
  done;
  let lu = Array.map Array.copy a0 in
  let p = Array.init n (fun i -> i) in
  for k = 0 to n - 1 do
    let piv = ref k in
    for i = k + 1 to n - 1 do
      if abs_float lu.(i).(k) > abs_float lu.(!piv).(k) then piv := i
    done;
    if !piv <> k then (
      let t = lu.(k) in
      lu.(k) <- lu.(!piv);
      lu.(!piv) <- t;
      let tp = p.(k) in
      p.(k) <- p.(!piv);
      p.(!piv) <- tp);
    for i = k + 1 to n - 1 do
      lu.(i).(k) <- lu.(i).(k) /. lu.(k).(k);
      for j = k + 1 to n - 1 do
        lu.(i).(j) <- lu.(i).(j) -. lu.(i).(k) *. lu.(k).(j)
      done
    done
  done;
  let bb = lusolve lu p rhs n in
  let r = Array.make n 0.0 in
  for i = 0 to n - 1 do
    let s = ref (-.rhs.(i)) in
    for j = 0 to n - 1 do
      s := !s +. a0.(i).(j) *. bb.(j)
    done;
    r.(i) <- !s
  done;
  let dx = lusolve lu p r n in
  for i = 0 to n - 1 do
    bb.(i) <- bb.(i) -. dx.(i)
  done;
  let b = Array.make (n + 1) 0.0 in
  b.(0) <- 1.0;
  for m = 1 to n do
    b.(m) <- bb.(m - 1)
  done;
  let av = Array.make (n + 1) 0.0 in
  av.(0) <- c.(0);
  for k = 1 to n do
    let s = ref 0.0 in
    for m = 0 to k do
      s := !s +. b.(m) *. c.(k - m)
    done;
    av.(k) <- !s
  done;
  for k = 0 to n do
    if k > 0 then print_string " ";
    print_string (fmtv av.(k))
  done;
  print_newline ();
  for m = 1 to n do
    if m > 1 then print_string " ";
    print_string (fmtv b.(m))
  done;
  print_newline ()
