(* Inverse of y = x log(x), smaller root (NR 6.11). *)

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
  let y = float_of_string toks.(0) in
  let ooe = 0.367879441171442322 in
  if not (y > -.ooe && y < 0.0) then print_endline "0.000000"
  else (
    let u = ref (if y < -0.2 then log (ooe -. sqrt (2.0 *. ooe *. (y +. ooe))) else -10.0)
    and to_ = ref 0.0
    and cont = ref true in
    while !cont do
      let t = (log (y /. !u) -. !u) *. (!u /. (1.0 +. !u)) in
      u := !u +. t;
      if t < 1.0e-8 && abs_float (t +. !to_) < 0.01 *. abs_float t then cont := false
      else (
        to_ := t;
        if not (abs_float (t /. !u) > 1.0e-15) then cont := false)
    done;
    print_endline (fmt6 (canon (exp !u))))
