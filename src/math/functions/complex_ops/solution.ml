(* Complex arithmetic (NR 5.5). Port of the C++ reference. *)

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
  let op = next () in
  let ar = float_of_string (next ()) in
  let ai = float_of_string (next ()) in
  let r = ref 0.0 and im = ref 0.0 in
  (if op = "DIV" then (
     let br = float_of_string (next ()) in
     let bi = float_of_string (next ()) in
     if abs_float br >= abs_float bi then (
       let t = bi /. br in
       let den = br +. bi *. t in
       r := (ar +. ai *. t) /. den;
       im := (ai -. ar *. t) /. den)
     else (
       let t = br /. bi in
       let den = bi +. br *. t in
       r := (ar *. t +. ai) /. den;
       im := (ai *. t -. ar) /. den))
   else if op = "SQRT" then (
     let w = ref 0.0 in
     (if ar <> 0.0 || ai <> 0.0 then
        if abs_float ar >= abs_float ai then (
          let t = ai /. ar in
          w := sqrt (abs_float ar) *. sqrt ((1.0 +. sqrt (1.0 +. t *. t)) /. 2.0))
        else (
          let t = ar /. ai in
          w := sqrt (abs_float ai) *. sqrt ((abs_float t +. sqrt (1.0 +. t *. t)) /. 2.0)));
     if !w = 0.0 then (
       r := 0.0;
       im := 0.0)
     else if ar >= 0.0 then (
       r := !w;
       im := ai /. (2.0 *. !w))
     else if ai >= 0.0 then (
       r := abs_float ai /. (2.0 *. !w);
       im := !w)
     else (
       r := abs_float ai /. (2.0 *. !w);
       im := -. !w))
   else if op = "EXP" then (
     let e = exp ar in
     r := e *. cos ai;
     im := e *. sin ai)
   else if op = "LOG" then (
     r := log (hypot ar ai);
     im := atan2 ai ar)
   else (
     let br = float_of_string (next ()) in
     let bi = float_of_string (next ()) in
     let lr = log (hypot ar ai) in
     let li = atan2 ai ar in
     let er = lr *. br -. li *. bi in
     let ei = lr *. bi +. li *. br in
     let e = exp er in
     r := e *. cos ei;
     im := e *. sin ei));
  print_string (fmt6 (canon !r));
  print_string " ";
  print_string (fmt6 (canon !im));
  print_newline ()
