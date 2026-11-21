(* Vandermonde solve in O(n^2) (NR 2.8.1, vander). Port of the C++ reference. *)

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
    let nums = List.map float_of_string toks in
    let arr = Array.of_list nums in
    let n = int_of_float arr.(0) in
    let x = Array.init n (fun i -> arr.(1 + i)) in
    let q = Array.init n (fun i -> arr.(1 + n + i)) in
    let w = Array.make n 0.0 in
    if n = 1 then w.(0) <- q.(0)
    else begin
      let c = Array.make n 0.0 in
      c.(n - 1) <- -. x.(0);
      for i = 1 to n - 1 do
        let xx = -. x.(i) in
        for j = n - 1 - i to n - 2 do c.(j) <- c.(j) +. xx *. c.(j + 1) done;
        c.(n - 1) <- c.(n - 1) +. xx
      done;
      for i = 0 to n - 1 do
        let xx = x.(i) in
        let t = ref 1.0 and b = ref 1.0 and s = ref q.(n - 1) in
        for k = n - 1 downto 1 do
          b := c.(k) +. xx *. !b;
          s := !s +. q.(k - 1) *. !b;
          t := xx *. !t +. !b
        done;
        w.(i) <- !s /. !t
      done
    end;
    print_endline (String.concat " " (List.init n (fun i -> fmt w.(i))))
