(* Polynomial and derivatives at a point (NR 5.1, ddpoly). *)

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
  let m = int_of_string (next ()) in
  let c = Array.init (n + 1) (fun _ -> float_of_string (next ())) in
  let x = float_of_string (next ()) in
  let pd = Array.make (m + 1) 0.0 in
  pd.(0) <- c.(n);
  for i = n - 1 downto 0 do
    let nnd = min m (n - i) in
    for j = nnd downto 1 do
      pd.(j) <- pd.(j) *. x +. pd.(j - 1)
    done;
    pd.(0) <- pd.(0) *. x +. c.(i)
  done;
  let cnst = ref 1.0 in
  for i = 2 to m do
    cnst := !cnst *. float i;
    pd.(i) <- pd.(i) *. !cnst
  done;
  for i = 0 to m do
    if i > 0 then print_string " ";
    print_string (fmt6 pd.(i))
  done;
  print_newline ()
