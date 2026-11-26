(* Continued fraction evaluation (NR 5.2, modified Lentz's method). *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

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
  let b0 = float_of_string (next ()) in
  let tiny = 1.0e-30 in
  let f = ref (if b0 = 0.0 then tiny else b0) in
  let c = ref !f and d = ref 0.0 in
  for _j = 1 to n do
    let a = float_of_string (next ()) in
    let b = float_of_string (next ()) in
    d := b +. a *. !d;
    if !d = 0.0 then d := tiny;
    c := b +. a /. !c;
    if !c = 0.0 then c := tiny;
    d := 1.0 /. !d;
    f := !f *. !c *. !d
  done;
  print_string (fmt6 (if abs_float !f < 5e-7 then 0.0 else !f));
  print_newline ()
