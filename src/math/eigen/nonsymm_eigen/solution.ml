(* Nonsymmetric eigensystem preprocessing toolkit: Osborne balancing
   (balance/balbak) and reduction to upper Hessenberg form by the
   elimination method (elmhes) with accumulated transformation (eltran).
   Reference: Press et al., Numerical Recipes 3e, Section 11.6.
   The QR iteration for Hessenberg matrices (11.7: hqr/hqr2/sort/sortvecs)
   is Webnote-only (Webnote 17), not in the book, so eigenvalues and
   eigenvectors of general nonsymmetric matrices are not computed here.
   Documented decisions: RADIX is hardcoded to 2.0 (the book derives
   numeric_limits<Doub>::radix, "2 for all modern machines"); perm[0] and
   perm[n-1] are unset by the book (only 1..n-2 defined), so we
   emit identity there for determinism (eltran never reads them).
   No online-judge problem found -- fixtures are self-generated
   (numpy oracle), no header link. *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

(* NOTE: float multiplies feeding +. or -. are let-bound first:
   ocamlopt fuses a direct mul-add/sub nest into FMA, which rounds
   once instead of twice and breaks bit-agreement with the C++
   reference (verified: zero FMA in the object code). *)

let balance a scale =
  let n = Array.length a in
  let radix = 2.0 in
  let sqrdx = radix *. radix in
  let fini = ref false in
  while not !fini do
    fini := true;
    for i = 0 to n - 1 do
      let r = ref 0.0 and c = ref 0.0 in
      for j = 0 to n - 1 do
        if j <> i then (
          c := !c +. abs_float a.(j).(i);
          r := !r +. abs_float a.(i).(j))
      done;
      if !c <> 0.0 && !r <> 0.0 then (
        let g = ref (!r /. radix) in
        let f = ref 1.0 in
        let s = !c +. !r in
        let c = ref !c in
        while !c < !g do
          f := !f *. radix;
          c := !c *. sqrdx
        done;
        g := !r *. radix;
        while !c > !g do
          f := !f /. radix;
          c := !c /. sqrdx
        done;
        if (!c +. !r) /. !f < 0.95 *. s then (
          fini := false;
          g := 1.0 /. !f;
          scale.(i) <- scale.(i) *. !f;
          for j = 0 to n - 1 do
            a.(i).(j) <- a.(i).(j) *. !g
          done;
          for j = 0 to n - 1 do
            a.(j).(i) <- a.(j).(i) *. !f
          done))
    done
  done

let balbak zz scale =
  let n = Array.length zz in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      zz.(i).(j) <- zz.(i).(j) *. scale.(i)
    done
  done

let elmhes a perm =
  let n = Array.length a in
  for m = 1 to n - 2 do
    let x = ref 0.0 and i = ref m in
    for j = m to n - 1 do
      if abs_float a.(j).(m - 1) > abs_float !x then (
        x := a.(j).(m - 1);
        i := j)
    done;
    perm.(m) <- !i;
    (if !i <> m then (
       for j = m - 1 to n - 1 do
         let t = a.(!i).(j) in
         a.(!i).(j) <- a.(m).(j);
         a.(m).(j) <- t
       done;
       for j = 0 to n - 1 do
         let t = a.(j).(!i) in
         a.(j).(!i) <- a.(j).(m);
         a.(j).(m) <- t
       done));
    if !x <> 0.0 then
      for i = m + 1 to n - 1 do
        let y = a.(i).(m - 1) in
        if y <> 0.0 then (
          let y = y /. !x in
          a.(i).(m - 1) <- y;
          for j = m to n - 1 do
            let p = y *. a.(m).(j) in
            a.(i).(j) <- a.(i).(j) -. p
          done;
          for j = 0 to n - 1 do
            let p = y *. a.(j).(i) in
            a.(j).(m) <- a.(j).(m) +. p
          done)
      done
  done

let eltran a perm zz =
  let n = Array.length a in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      zz.(i).(j) <- (if i = j then 1.0 else 0.0)
    done
  done;
  for mp = n - 2 downto 1 do
    for k = mp + 1 to n - 1 do
      zz.(k).(mp) <- a.(k).(mp - 1)
    done;
    let i = perm.(mp) in
    if i <> mp then (
      for j = mp to n - 1 do
        zz.(mp).(j) <- zz.(i).(j);
        zz.(i).(j) <- 0.0
      done;
      zz.(i).(mp) <- 1.0)
  done

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
  let op = toks.(0) in
  let n = int_of_string toks.(1) in
  let p = ref 2 in
  let read_mat () =
    let a = Array.make_matrix n n 0.0 in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        a.(i).(j) <- float_of_string toks.(!p);
        incr p
      done
    done;
    a
  in
  if op = "BALANCE" then (
    let a = read_mat () in
    let scale = Array.make n 1.0 in
    balance a scale;
    let parts = ref [] in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        parts := fmt6 (canon a.(i).(j)) :: !parts
      done
    done;
    for i = 0 to n - 1 do
      parts := fmt6 (canon scale.(i)) :: !parts
    done;
    print_endline (String.concat " " (List.rev !parts)))
  else if op = "HESSENBERG" then (
    let a = read_mat () in
    let perm = Array.init n (fun i -> i) in
    elmhes a perm;
    let parts = ref [] in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        parts := fmt6 (canon a.(i).(j)) :: !parts
      done
    done;
    for i = 0 to n - 1 do
      parts := string_of_int perm.(i) :: !parts
    done;
    print_endline (String.concat " " (List.rev !parts)))
  else if op = "ELTRAN" then (
    let a = read_mat () in
    let perm = Array.init n (fun _ ->
      let v = int_of_string toks.(!p) in
      incr p;
      v) in
    let zz = Array.make_matrix n n 0.0 in
    eltran a perm zz;
    let parts = ref [] in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        parts := fmt6 (canon zz.(i).(j)) :: !parts
      done
    done;
    print_endline (String.concat " " (List.rev !parts)))
  else if op = "BALBAK" then (
    let zz = read_mat () in
    let scale = Array.init n (fun _ ->
      let v = float_of_string toks.(!p) in
      incr p;
      v) in
    balbak zz scale;
    let parts = ref [] in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        parts := fmt6 (canon zz.(i).(j)) :: !parts
      done
    done;
    print_endline (String.concat " " (List.rev !parts)))
