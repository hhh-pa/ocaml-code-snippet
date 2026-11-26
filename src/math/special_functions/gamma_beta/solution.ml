(* Gamma/beta/factorial/binomial (NR 6.1). *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

let gcof =
  [| 57.1562356658629235; -59.5979603554754912; 14.1360979747417471;
     -0.491913816097620199; 0.339946499848118887e-4; 0.465236289270485756e-4;
     -0.983744753048795646e-4; 0.158088703224912494e-3; -0.210264441724104883e-3;
     0.217439618115212643e-3; -0.164318106536763890e-3; 0.844182239838527433e-4;
     -0.261908384015814087e-4; 0.368991826595316234e-5 |]

let gammln xx =
  let y = ref xx in
  let x = xx in
  let tmp = ref (x +. 5.2421875000000000) in
  tmp := (x +. 0.5) *. log !tmp -. !tmp;
  let ser = ref 0.999999999999997092 in
  for j = 0 to 13 do
    y := !y +. 1.0;
    ser := !ser +. gcof.(j) /. !y
  done;
  !tmp +. log (2.5066282746310005 *. !ser /. x)

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
  let fact = Array.make 171 0.0 in
  fact.(0) <- 1.0;
  for i = 1 to 170 do
    fact.(i) <- float i *. fact.(i - 1)
  done;
  let factl = Array.init 2000 (fun i -> gammln (float i +. 1.0)) in
  let ans =
    if op = "GAMMALN" then gammln (float_of_string (next_tok ()))
    else if op = "GAMMA" then exp (gammln (float_of_string (next_tok ())))
    else if op = "BETA" then (
      let z = float_of_string (next_tok ()) in
      let w = float_of_string (next_tok ()) in
      exp (gammln z +. gammln w -. gammln (z +. w)))
    else if op = "FACT" then fact.(int_of_string (next_tok ()))
    else if op = "FACTLN" then (
      let n = int_of_string (next_tok ()) in
      if n < 2000 then factl.(n) else gammln (float n +. 1.0))
    else (
      let n = int_of_string (next_tok ()) in
      let k = int_of_string (next_tok ()) in
      if n < 171 then floor (0.5 +. fact.(n) /. (fact.(k) *. fact.(n - k)))
      else floor (0.5 +. exp (factl.(n) -. factl.(k) -. factl.(n - k))))
  in
  print_endline (fmt6 (canon ans))
