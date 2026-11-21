(* Shepard interpolation (NR 3.7.4). Port of the C++ reference. *)

let fmt v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let () =
  let toks = String.split_on_char ' ' (String.concat " " (let rec rd acc =
    try rd (input_line stdin :: acc) with End_of_file -> List.rev acc in rd [])) in
  let toks = List.filter (fun s -> s <> "") toks in
  match toks with
  | [] -> ()
  | _ ->
    let p = int_of_string (List.nth toks 0) in
    let n = int_of_string (List.nth toks 1) in
    let dim = int_of_string (List.nth toks 2) in
    let nums = List.map float_of_string (List.tl (List.tl (List.tl toks))) in
    let arr = Array.of_list nums in
    let pts = Array.init n (fun i -> Array.init dim (fun j -> arr.(i * dim + j))) in
    let vals = Array.init n (fun i -> arr.(n * dim + i)) in
    let q = int_of_float arr.(n * dim + n) in
    for qq = 0 to q - 1 do
      let pt = Array.init dim (fun j -> arr.(n * dim + n + 1 + qq * dim + j)) in
      let num = ref 0.0 and den = ref 0.0 and exact = ref (-1) in
      for i = 0 to n - 1 do
        let s = ref 0.0 in
        for j = 0 to dim - 1 do s := !s +. (pt.(j) -. pts.(i).(j)) *. (pt.(j) -. pts.(i).(j)) done;
        let r = sqrt !s in
        if r = 0.0 then exact := i
        else begin
          let rp = ref 1.0 in
          for _k = 0 to p - 1 do rp := !rp *. r done;
          let w = 1.0 /. !rp in
          num := !num +. w *. vals.(i);
          den := !den +. w
        end
      done;
      if !exact >= 0 then print_endline (fmt vals.(!exact))
      else print_endline (fmt (!num /. !den))
    done
