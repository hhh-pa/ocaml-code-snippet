(* Band-diagonal linear solver (NR 2.4): Bandec LU with partial pivoting
   plus determinant., following the NR algorithm verbatim. *)

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
    let n = int_of_float arr.(0) in
    let m1 = int_of_float arr.(1) in
    let m2 = int_of_float arr.(2) in
    let full = Array.init n (fun i -> Array.init n (fun j -> arr.(3 + i * n + j))) in
    let b = if op = "SOLVE" then Array.init n (fun i -> arr.(3 + n * n + i))
      else Array.make n 0.0 in
    let mm = m1 + m2 + 1 in
    let au = Array.make_matrix n mm 0.0 in
    for i = 0 to n - 1 do
      for k = max 0 (i - m1) to min (n - 1) (i + m2) do
        au.(i).(k - i + m1) <- full.(i).(k)
      done
    done;
    let al = Array.make_matrix n (max m1 1) 0.0 in
    let indx = Array.make n 0 in
    let tiny = 1.0e-40 in
    let l = ref m1 in
    for i = 0 to m1 - 1 do
      for j = m1 - i to mm - 1 do au.(i).(j - !l) <- au.(i).(j) done;
      decr l;
      for j = mm - !l - 1 to mm - 1 do au.(i).(j) <- 0.0 done
    done;
    let d = ref 1.0 in
    l := m1;
    for k = 0 to n - 1 do
      let dum = ref au.(k).(0) in
      let i = ref k in
      if !l < n then incr l;
      for j = k + 1 to !l - 1 do
        if abs_float au.(j).(0) > abs_float !dum then begin dum := au.(j).(0); i := j end
      done;
      indx.(k) <- !i + 1;
      if !dum = 0.0 then au.(k).(0) <- tiny;
      if !i <> k then begin
        d := -. !d;
        for j = 0 to mm - 1 do
          let t = au.(k).(j) in au.(k).(j) <- au.(!i).(j); au.(!i).(j) <- t
        done
      end;
      for ii = k + 1 to !l - 1 do
        dum := au.(ii).(0) /. au.(k).(0);
        al.(k).(ii - k - 1) <- !dum;
        for j = 1 to mm - 1 do au.(ii).(j - 1) <- au.(ii).(j) -. !dum *. au.(k).(j) done;
        au.(ii).(mm - 1) <- 0.0
      done
    done;
    if op = "DET" then begin
      let dd = ref !d in
      for i = 0 to n - 1 do dd := !dd *. au.(i).(0) done;
      print_endline (fmt !dd)
    end else begin
      let x = Array.copy b in
      l := m1;
      for k = 0 to n - 1 do
        let j = indx.(k) - 1 in
        if j <> k then begin let t = x.(k) in x.(k) <- x.(j); x.(j) <- t end;
        if !l < n then incr l;
        for j = k + 1 to !l - 1 do x.(j) <- x.(j) -. al.(k).(j - k - 1) *. x.(k) done
      done;
      l := 1;
      for i = n - 1 downto 0 do
        let dum = ref x.(i) in
        for k = 1 to !l - 1 do dum := !dum -. au.(i).(k) *. x.(k + i) done;
        x.(i) <- !dum /. au.(i).(0);
        if !l < mm then incr l
      done;
      print_endline (String.concat " " (List.map fmt (Array.to_list x)))
    end
