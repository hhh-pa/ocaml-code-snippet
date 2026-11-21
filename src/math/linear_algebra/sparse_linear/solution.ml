(* Sparse linear systems (NR 2.7): CCS matvecs + BiCG.
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
    let n = int_of_string (List.nth rest 0) in
    let nnz = int_of_string (List.nth rest 1) in
    let raw = Array.of_list (List.tl (List.tl rest)) in
    let trips = List.init nnz (fun k ->
      (int_of_string raw.(k * 3), int_of_string raw.(k * 3 + 1), float_of_string raw.(k * 3 + 2))) in
    let trips = List.sort (fun (i1, j1, _) (i2, j2, _) ->
      if j1 <> j2 then compare j1 j2 else compare i1 i2) trips in
    let col_ptr = Array.make (n + 1) 0 in
    let row_ind = Array.make nnz 0 in
    let vals = Array.make nnz 0.0 in
    List.iter (fun (_, j, _) -> col_ptr.(j + 1) <- col_ptr.(j + 1) + 1) trips;
    for j = 0 to n - 1 do col_ptr.(j + 1) <- col_ptr.(j + 1) + col_ptr.(j) done;
    let next = Array.copy col_ptr in
    List.iter (fun (i, j, v) ->
      let k = next.(j) in next.(j) <- k + 1; row_ind.(k) <- i; vals.(k) <- v) trips;
    let ax x =
      let y = Array.make n 0.0 in
      for j = 0 to n - 1 do
        for i = col_ptr.(j) to col_ptr.(j + 1) - 1 do
          y.(row_ind.(i)) <- y.(row_ind.(i)) +. vals.(i) *. x.(j)
        done
      done;
      y in
    let atx x =
      let y = Array.make n 0.0 in
      for i = 0 to n - 1 do
        y.(i) <- 0.0;
        for j = col_ptr.(i) to col_ptr.(i + 1) - 1 do
          y.(i) <- y.(i) +. vals.(j) *. x.(row_ind.(j))
        done
      done;
      y in
    let diag = Array.make n 0.0 in
    for i = 0 to n - 1 do
      let found = ref false in
      for j = col_ptr.(i) to col_ptr.(i + 1) - 1 do
        if (not !found) && row_ind.(j) = i then begin diag.(i) <- vals.(j); found := true end
      done
    done;
    let asolve b =
      Array.init n (fun i -> if diag.(i) <> 0.0 then b.(i) /. diag.(i) else b.(i)) in
    let dot a b =
      let s = ref 0.0 in
      for i = 0 to n - 1 do s := !s +. a.(i) *. b.(i) done;
      !s in
    let rest2 = Array.sub raw (nnz * 3) (Array.length raw - nnz * 3) in
    if op = "MATVEC" || op = "TMATVEC" then begin
      let x = Array.init n (fun i -> float_of_string rest2.(i)) in
      let y = if op = "MATVEC" then ax x else atx x in
      print_endline (String.concat " " (List.init n (fun i -> fmt y.(i))))
    end else begin
      let b = Array.init n (fun i -> float_of_string rest2.(i)) in
      let itmax = int_of_string rest2.(n) in
      let x = Array.make n 0.0 in
      let p = Array.make n 0.0 in
      let pp = Array.make n 0.0 in
      let ax0 = ax x in
      let r = Array.init n (fun j -> b.(j) -. ax0.(j)) in
      let rr = Array.copy r in
      let z = ref (asolve r) in
      let bkden = ref 1.0 in
      for iter = 1 to itmax do
        let zz = asolve rr in
        let bknum = dot !z rr in
        if iter = 1 then begin
          Array.blit !z 0 p 0 n; Array.blit zz 0 pp 0 n
        end else begin
          let bk = if !bkden = 0.0 then 0.0 else bknum /. !bkden in
          for j = 0 to n - 1 do
            p.(j) <- bk *. p.(j) +. (!z).(j);
            pp.(j) <- bk *. pp.(j) +. zz.(j)
          done
        end;
        bkden := bknum;
        z := ax p;
        let akden = dot !z pp in
        let ak = if akden = 0.0 then 0.0 else bknum /. akden in
        let zz = atx pp in
        for j = 0 to n - 1 do
          x.(j) <- x.(j) +. ak *. p.(j);
          r.(j) <- r.(j) -. ak *. (!z).(j);
          rr.(j) <- rr.(j) -. ak *. zz.(j)
        done;
        z := asolve r
      done;
      let axf = ax x in
      let rn = ref 0.0 in
      for j = 0 to n - 1 do
        let d = b.(j) -. axf.(j) in rn := !rn +. d *. d
      done;
      print_endline (String.concat " " (List.init n (fun i -> fmt x.(i))));
      print_endline (fmt (sqrt !rn))
    end
