(* Spherical harmonics (NR 6.7). *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v
let pi = 3.141592653589793

let plegendre l m x =
  let pmm = ref 1.0 in
  (if m > 0 then (
     let omx2 = (1.0 -. x) *. (1.0 +. x) in
     let fact = ref 1.0 in
     for _i = 1 to m do
       pmm := !pmm *. omx2 *. !fact /. (!fact +. 1.0);
       fact := !fact +. 2.0
     done));
  pmm := sqrt (float (2 * m + 1) *. !pmm /. (4.0 *. pi));
  if m mod 2 <> 0 then pmm := -. !pmm;
  if l = m then !pmm
  else (
    let pmmp1 = ref (x *. sqrt (2.0 *. float m +. 3.0) *. !pmm) in
    if l = m + 1 then !pmmp1
    else (
      let oldfact = ref (sqrt (2.0 *. float m +. 3.0)) in
      let pll = ref 0.0 in
      for ll = m + 2 to l do
        let fact =
          sqrt
            ((4.0 *. float ll *. float ll -. 1.0)
            /. (float ll *. float ll -. float m *. float m))
        in
        pll := (x *. !pmmp1 -. !pmm /. !oldfact) *. fact;
        oldfact := fact;
        pmm := !pmmp1;
        pmmp1 := !pll
      done;
      !pll))

let plgndr l m x =
  let prod = ref 1.0 in
  for j = l - m + 1 to l + m do
    prod := !prod *. float j
  done;
  sqrt (4.0 *. pi *. !prod /. float (2 * l + 1)) *. plegendre l m x

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
  if op = "PLEG" || op = "PLM" then (
    let l = int_of_string (next_tok ()) in
    let m = int_of_string (next_tok ()) in
    let x = float_of_string (next_tok ()) in
    let ans = if op = "PLEG" then plegendre l m x else plgndr l m x in
    print_endline (fmt6 (canon ans)))
  else (
    let l = int_of_string (next_tok ()) in
    let m = int_of_string (next_tok ()) in
    let th = float_of_string (next_tok ()) in
    let ph = float_of_string (next_tok ()) in
    let re, im =
      if m >= 0 then (
        let pt = plegendre l m (cos th) in
        (pt *. cos (float m *. ph), pt *. sin (float m *. ph)))
      else (
        let pt = plegendre l (-m) (cos th) in
        let s = if (-m) mod 2 <> 0 then -1.0 else 1.0 in
        (s *. pt *. cos (float m *. ph), s *. pt *. sin (float m *. ph)))
    in
    print_string (fmt6 (canon re));
    print_string " ";
    print_string (fmt6 (canon im));
    print_newline ())
