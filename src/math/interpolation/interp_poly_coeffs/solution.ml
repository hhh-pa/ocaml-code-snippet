(* Coefficients of the interpolating polynomial (NR 3.5, polcof). *)

let fmt v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let neville xa ya m x =
  let c = Array.init m (fun i -> ya.(i)) in
  let d = Array.init m (fun i -> ya.(i)) in
  let dif = ref (abs_float (x -. xa.(0))) in
  let ns = ref 0 in
  for i = 0 to m - 1 do
    let dift = abs_float (x -. xa.(i)) in
    if dift < !dif then begin ns := i; dif := dift end
  done;
  let y = ref ya.(!ns) in
  decr ns;
  for mm = 1 to m - 1 do
    for i = 0 to m - mm - 1 do
      let ho = xa.(i) -. x in
      let hp = xa.(i + mm) -. x in
      let w = c.(i + 1) -. d.(i) in
      let den = w /. (ho -. hp) in
      d.(i) <- hp *. den;
      c.(i) <- ho *. den
    done;
    if 2 * (!ns + 1) < (m - mm) then y := !y +. c.(!ns + 1)
    else begin y := !y +. d.(!ns); decr ns end
  done;
  !y

let () =
  let toks = String.split_on_char ' ' (String.concat " " (let rec rd acc =
    try rd (input_line stdin :: acc) with End_of_file -> List.rev acc in rd [])) in
  let toks = List.filter (fun s -> s <> "") toks in
  match toks with
  | [] -> ()
  | _ ->
    let n = int_of_string (List.nth toks 0) in
    let nums = List.map float_of_string (List.tl toks) in
    let arr = Array.of_list nums in
    let x = ref (Array.init n (fun i -> arr.(i))) in
    let y = ref (Array.init n (fun i -> arr.(n + i))) in
    let cof = Array.make n 0.0 in
    for j = 0 to n - 1 do
      let m = Array.length !x in
      let cj = neville !x !y m 0.0 in
      cof.(j) <- cj;
      let xmin = ref 1.0e99 and k = ref (-1) in
      for i = 0 to m - 1 do
        if abs_float (!x).(i) < !xmin then begin xmin := abs_float (!x).(i); k := i end;
        if (!x).(i) <> 0.0 then (!y).(i) <- ((!y).(i) -. cj) /. (!x).(i)
      done;
      let nx = Array.make (m - 1) 0.0 and ny = Array.make (m - 1) 0.0 in
      let w = ref 0 in
      for i = 0 to m - 1 do
        if i <> !k then begin nx.(!w) <- (!x).(i); ny.(!w) <- (!y).(i); incr w end
      done;
      x := nx; y := ny
    done;
    print_endline (String.concat " " (List.init n (fun i -> fmt cof.(i))))
