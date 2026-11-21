(* Clenshaw's recurrence (NR 5.4). Port of the C++ reference. *)

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
  let f0 = float_of_string (next ()) in
  let f1 = float_of_string (next ()) in
  let c = Array.init (n + 1) (fun _ -> float_of_string (next ())) in
  let al = Array.make (n + 1) 0.0 in
  for i = 1 to n do
    al.(i) <- float_of_string (next ())
  done;
  let be = Array.make (n + 2) 0.0 in
  for i = 1 to n + 1 do
    be.(i) <- float_of_string (next ())
  done;
  let y = Array.make (n + 3) 0.0 in
  for k = n downto 1 do
    y.(k) <- al.(k) *. y.(k + 1) +. be.(k + 1) *. y.(k + 2) +. c.(k)
  done;
  let f = be.(1) *. f0 *. y.(2) +. f1 *. y.(1) +. f0 *. c.(0) in
  print_string (fmt6 (if abs_float f < 5e-7 then 0.0 else f));
  print_newline ()
