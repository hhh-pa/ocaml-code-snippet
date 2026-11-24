(* Exponential integrals (NR 6.3). *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v
let euler = 0.577215664901533
let eps = 2.220446049250313e-16
let big = 1.7976931348623157e308 *. eps
let fpmin = 1.0020841800044864e-292

let expint n x =
  let maxit = 100 in
  let nm1 = n - 1 in
  if n = 0 then exp (-.x) /. x
  else if x = 0.0 then 1.0 /. float nm1
  else if x > 1.0 then (
    let b = ref (x +. float n) and c = ref big and d = ref 0.0 and h = ref 0.0 in
    d := 1.0 /. !b;
    h := !d;
    (try
       for i = 1 to maxit do
         let a = -.float i *. (float nm1 +. float i) in
         b := !b +. 2.0;
         d := 1.0 /. (a *. !d +. !b);
         c := !b +. a /. !c;
         let del = !c *. !d in
         h := !h *. del;
         if abs_float (del -. 1.0) <= eps then raise Exit
       done
     with Exit -> ());
    !h *. exp (-.x))
  else (
    let ans = ref (if nm1 <> 0 then 1.0 /. float nm1 else -.log x -. euler) in
    let fact = ref 1.0 in
    (try
       for i = 1 to maxit do
         fact := !fact *. -.x /. float i;
         let del =
           if i <> nm1 then -. !fact /. (float i -. float nm1)
           else (
             let psi = ref (-.euler) in
             for ii = 1 to nm1 do
               psi := !psi +. 1.0 /. float ii
             done;
             !fact *. (-.log x +. !psi))
         in
         ans := !ans +. del;
         if abs_float del < abs_float !ans *. eps then raise Exit
       done
     with Exit -> ());
    !ans)

let eifunc x =
  let maxit = 100 in
  if x < fpmin then log x +. euler
  else if x <= -.log eps then (
    let sum = ref 0.0 and fact = ref 1.0 in
    (try
       for k = 1 to maxit do
         fact := !fact *. x /. float k;
         let term = !fact /. float k in
         sum := !sum +. term;
         if term < eps *. !sum then raise Exit
       done
     with Exit -> ());
    !sum +. log x +. euler)
  else (
    let sum = ref 0.0 and term = ref 1.0 in
    (try
       for k = 1 to maxit do
         let prev = !term in
         term := !term *. float k /. x;
         if !term < eps then raise Exit;
         if !term < prev then sum := !sum +. !term
         else (
           sum := !sum -. prev;
           raise Exit)
       done
     with Exit -> ());
    exp x *. (1.0 +. !sum) /. x)

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
  let ans =
    if op = "EN" then (
      let ns = next_tok () in
      let xs = next_tok () in
      expint (int_of_string ns) (float_of_string xs))
    else eifunc (float_of_string (next_tok ()))
  in
  print_endline (fmt6 (canon ans))
