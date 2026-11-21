(* Polynomial interpolation by Neville's algorithm (NR 3.2). Port of the C++ reference. *)

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
    let n = int_of_string (List.nth toks 0) in
    let nums = List.map float_of_string (List.tl toks) in
    let arr = Array.of_list nums in
    let xa = Array.init n (fun i -> arr.(i)) in
    let ya = Array.init n (fun i -> arr.(n + i)) in
    let q = int_of_float arr.(2 * n) in
    let qs = Array.init q (fun i -> arr.(2 * n + 1 + i)) in
    for qq = 0 to q - 1 do
      let x = qs.(qq) in
      let c = Array.copy ya in
      let d = Array.copy ya in
      let dif = ref (abs_float (x -. xa.(0))) in
      let ns = ref 0 in
      for i = 0 to n - 1 do
        let dift = abs_float (x -. xa.(i)) in
        if dift < !dif then begin ns := i; dif := dift end
      done;
      let y = ref ya.(!ns) in
      decr ns;
      let dy = ref 0.0 in
      for m = 1 to n - 1 do
        for i = 0 to n - m - 1 do
          let ho = xa.(i) -. x in
          let hp = xa.(i + m) -. x in
          let w = c.(i + 1) -. d.(i) in
          let den = w /. (ho -. hp) in
          d.(i) <- hp *. den;
          c.(i) <- ho *. den
        done;
        if 2 * (!ns + 1) < (n - m) then dy := c.(!ns + 1)
        else begin dy := d.(!ns); decr ns end;
        y := !y +. !dy
      done;
      Printf.printf "%s %s\n" (fmt !y) (fmt !dy)
    done
