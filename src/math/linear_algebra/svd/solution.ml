(* Singular value decomposition (NR 2.6 contract): one-sided Jacobi.
   Port of the C++ reference. *)

let fmt v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let () =
  let toks = String.split_on_char ' ' (String.concat " " (let rec rd acc =
    try rd (input_line stdin :: acc) with End_of_file -> List.rev acc in rd [])) in
  let toks = List.filter (fun s -> s <> "") toks in
  match toks with
  | [] -> ()
  | op :: rest ->
    let nums = List.map float_of_string rest in
    let arr = Array.of_list nums in
    let m = int_of_float arr.(0) in
    let n = int_of_float arr.(1) in
    let a = Array.init m (fun i -> Array.init n (fun j -> arr.(2 + i * n + j))) in
    let b = Array.make m 0.0 in
    let thresh = ref 0.0 in
    if op = "SOLVE" then begin
      for i = 0 to m - 1 do b.(i) <- arr.(2 + m * n + i) done;
      thresh := arr.(2 + m * n + m)
    end;
    if op = "RANK" || op = "SUBSPACE" then thresh := arr.(2 + m * n);
    let bb = Array.init m (fun i -> Array.copy a.(i)) in
    let v = Array.make_matrix n n 0.0 in
    for i = 0 to n - 1 do v.(i).(i) <- 1.0 done;
    for _sweep = 0 to 11 do
      for p = 0 to n - 2 do
        for q = p + 1 to n - 1 do
          let alpha = ref 0.0 and gamma = ref 0.0 and beta = ref 0.0 in
          for k = 0 to m - 1 do
            alpha := !alpha +. bb.(k).(p) *. bb.(k).(p);
            gamma := !gamma +. bb.(k).(q) *. bb.(k).(q);
            beta := !beta +. bb.(k).(p) *. bb.(k).(q)
          done;
          if !beta <> 0.0 then begin
            let zeta = (!gamma -. !alpha) /. (2.0 *. !beta) in
            let t = (if zeta < 0.0 then -1.0 else 1.0) /. (abs_float zeta +. sqrt (1.0 +. zeta *. zeta)) in
            let c = 1.0 /. sqrt (1.0 +. t *. t) in
            let s = c *. t in
            for k = 0 to m - 1 do
              let bp = bb.(k).(p) and bq = bb.(k).(q) in
              bb.(k).(p) <- c *. bp -. s *. bq;
              bb.(k).(q) <- s *. bp +. c *. bq
            done;
            for k = 0 to n - 1 do
              let vp = v.(k).(p) and vq = v.(k).(q) in
              v.(k).(p) <- c *. vp -. s *. vq;
              v.(k).(q) <- s *. vp +. c *. vq
            done
          end
        done
      done
    done;
    let w = Array.make n 0.0 in
    let u = Array.make_matrix m n 0.0 in
    for j = 0 to n - 1 do
      let s = ref 0.0 in
      for k = 0 to m - 1 do s := !s +. bb.(k).(j) *. bb.(k).(j) done;
      w.(j) <- sqrt !s;
      if w.(j) <> 0.0 then
        for k = 0 to m - 1 do u.(k).(j) <- bb.(k).(j) /. w.(j) done
    done;
    for i = 0 to n - 2 do
      let best = ref i in
      for j = i + 1 to n - 1 do
        if w.(j) > w.(!best) then best := j
      done;
      if !best <> i then begin
        let t = w.(i) in w.(i) <- w.(!best); w.(!best) <- t;
        for k = 0 to m - 1 do
          let t = u.(k).(i) in u.(k).(i) <- u.(k).(!best); u.(k).(!best) <- t
        done;
        for k = 0 to n - 1 do
          let t = v.(k).(i) in v.(k).(i) <- v.(k).(!best); v.(k).(!best) <- t
        done
      end
    done;
    if op = "FACTOR" then begin
      print_endline "U";
      for i = 0 to m - 1 do
        print_endline (String.concat " " (List.init n (fun j -> fmt u.(i).(j))))
      done;
      print_endline "W";
      print_endline (String.concat " " (List.init n (fun j -> fmt w.(j))));
      print_endline "V";
      for i = 0 to n - 1 do
        print_endline (String.concat " " (List.init n (fun j -> fmt v.(i).(j))))
      done
    end else if op = "SOLVE" then begin
      let tmp = Array.make n 0.0 in
      for j = 0 to n - 1 do
        if w.(j) > !thresh then begin
          let s = ref 0.0 in
          for i = 0 to m - 1 do s := !s +. u.(i).(j) *. b.(i) done;
          tmp.(j) <- !s /. w.(j)
        end
      done;
      let x = Array.make n 0.0 in
      for j = 0 to n - 1 do
        let s = ref 0.0 in
        for jj = 0 to n - 1 do s := !s +. v.(j).(jj) *. tmp.(jj) done;
        x.(j) <- !s
      done;
      print_endline (String.concat " " (List.init n (fun j -> fmt x.(j))))
    end else if op = "RANK" then begin
      let rank = ref 0 in
      for j = 0 to n - 1 do if w.(j) > !thresh then incr rank done;
      Printf.printf "%d %d\n" !rank (n - !rank)
    end else if op = "SUBSPACE" then begin
      let rg = ref [] and ns = ref [] in
      for j = 0 to n - 1 do
        if w.(j) > !thresh then rg := j :: !rg else ns := j :: !ns
      done;
      let rg = List.rev !rg and ns = List.rev !ns in
      Printf.printf "RANGE %d\n" (List.length rg);
      List.iter (fun j ->
        print_endline (String.concat " " (List.init m (fun i -> fmt u.(i).(j))))) rg;
      Printf.printf "NULLSPACE %d\n" (List.length ns);
      List.iter (fun j ->
        print_endline (String.concat " " (List.init n (fun i -> fmt v.(i).(j))))) ns
    end else begin
      let cnd = if w.(0) <= 0.0 || w.(n - 1) <= 0.0 then 0.0 else w.(n - 1) /. w.(0) in
      print_endline (fmt cnd)
    end
