(* Grid interpolation (NR 3.6). *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let locate xx x =
  let n = Array.length xx in
  let klo = ref 0 and khi = ref (n - 1) in
  while !khi - !klo > 1 do
    let k = (!khi + !klo) / 2 in
    if xx.(k) > x then khi := k else klo := k
  done;
  if !klo < 0 then klo := 0;
  if !klo > n - 2 then klo := n - 2;
  !klo

let neville xa ya m x =
  let c = Array.copy ya and d = Array.copy ya in
  let dif = ref (abs_float (x -. xa.(0))) in
  let ns = ref 0 in
  for i = 0 to m - 1 do
    let dift = abs_float (x -. xa.(i)) in
    if dift < !dif then (
      ns := i;
      dif := dift);
    c.(i) <- ya.(i);
    d.(i) <- ya.(i)
  done;
  let y = ref ya.(!ns) in
  ns := !ns - 1;
  for mm = 1 to m - 1 do
    for i = 0 to m - mm - 1 do
      let ho = xa.(i) -. x and hp = xa.(i + mm) -. x in
      let w = c.(i + 1) -. d.(i) in
      let den = w /. (ho -. hp) in
      d.(i) <- hp *. den;
      c.(i) <- ho *. den
    done;
    if 2 * (!ns + 1) < m - mm then y := !y +. c.(!ns + 1)
    else (
      y := !y +. d.(!ns);
      ns := !ns - 1)
  done;
  !y

let spline_y2 xv yv =
  let n = Array.length xv in
  let y2 = Array.make n 0.0 and u = Array.make (n - 1) 0.0 in
  y2.(0) <- 0.0;
  u.(0) <- 0.0;
  for i = 1 to n - 2 do
    let sg = (xv.(i) -. xv.(i - 1)) /. (xv.(i + 1) -. xv.(i - 1)) in
    let p = sg *. y2.(i - 1) +. 2.0 in
    y2.(i) <- (sg -. 1.0) /. p;
    let ui =
      (yv.(i + 1) -. yv.(i)) /. (xv.(i + 1) -. xv.(i))
      -. (yv.(i) -. yv.(i - 1)) /. (xv.(i) -. xv.(i - 1))
    in
    u.(i) <- (6.0 *. ui /. (xv.(i + 1) -. xv.(i - 1)) -. sg *. u.(i - 1)) /. p
  done;
  y2.(n - 1) <- (0.0 -. 0.0 *. u.(n - 2)) /. (0.0 *. y2.(n - 2) +. 1.0);
  for k = n - 2 downto 0 do
    y2.(k) <- y2.(k) *. y2.(k + 1) +. u.(k)
  done;
  y2

let wt =
  [| [| 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0 |];
      [| -3; 0; 0; 3; 0; 0; 0; 0; -2; 0; 0; -1; 0; 0; 0; 0 |];
      [| 2; 0; 0; -2; 0; 0; 0; 0; 1; 0; 0; 1; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0 |];
      [| 0; 0; 0; 0; -3; 0; 0; 3; 0; 0; 0; 0; -2; 0; 0; -1 |];
      [| 0; 0; 0; 0; 2; 0; 0; -2; 0; 0; 0; 0; 1; 0; 0; 1 |];
      [| -3; 3; 0; 0; -2; -1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0; -3; 3; 0; 0; -2; -1; 0; 0 |];
      [| 9; -9; 9; -9; 6; 3; -3; -6; 6; -6; -3; 3; 4; 2; 1; 2 |];
      [| -6; 6; -6; 6; -4; -2; 2; 4; -3; 3; 3; -3; -2; -1; -1; -2 |];
      [| 2; -2; 0; 0; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0; 2; -2; 0; 0; 1; 1; 0; 0 |];
      [| -6; 6; -6; 6; -3; -3; 3; 3; -4; 4; 2; -2; -2; -2; -1; -1 |];
      [| 4; -4; 4; -4; 2; 2; -2; -2; 2; -2; -2; 2; 1; 1; 1; 1 |] |]

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
  if op = "BICUBIC" then (
    let x1l = float_of_string (next ()) in
    let x1u = float_of_string (next ()) in
    let x2l = float_of_string (next ()) in
    let x2u = float_of_string (next ()) in
    let y = Array.make 4 0.0
    and y1 = Array.make 4 0.0
    and y2 = Array.make 4 0.0
    and y12 = Array.make 4 0.0 in
    for i = 0 to 3 do
      y.(i) <- float_of_string (next ())
    done;
    for i = 0 to 3 do
      y1.(i) <- float_of_string (next ())
    done;
    for i = 0 to 3 do
      y2.(i) <- float_of_string (next ())
    done;
    for i = 0 to 3 do
      y12.(i) <- float_of_string (next ())
    done;
    let q = int_of_string (next ()) in
    let d1 = x1u -. x1l and d2 = x2u -. x2l in
    let d1d2 = d1 *. d2 in
    let x = Array.make 16 0.0 and cl = Array.make 16 0.0 in
    let c = Array.make_matrix 4 4 0.0 in
    for i = 0 to 3 do
      x.(i) <- y.(i);
      x.(i + 4) <- y1.(i) *. d1;
      x.(i + 8) <- y2.(i) *. d2;
      x.(i + 12) <- y12.(i) *. d1d2
    done;
    for i = 0 to 15 do
      let xx = ref 0.0 in
      for k = 0 to 15 do
        xx := !xx +. float wt.(i).(k) *. x.(k)
      done;
      cl.(i) <- !xx
    done;
    let l = ref 0 in
    for i = 0 to 3 do
      for j = 0 to 3 do
        c.(i).(j) <- cl.(!l);
        incr l
      done
    done;
    for _qq = 0 to q - 1 do
      let x1 = float_of_string (next ()) in
      let x2 = float_of_string (next ()) in
      let t = (x1 -. x1l) /. d1 and u = (x2 -. x2l) /. d2 in
      let ansy = ref 0.0 and ansy1 = ref 0.0 and ansy2 = ref 0.0 in
      for i = 3 downto 0 do
        ansy := t *. !ansy +. ((c.(i).(3) *. u +. c.(i).(2)) *. u +. c.(i).(1)) *. u +. c.(i).(0);
        ansy2 := t *. !ansy2 +. (3.0 *. c.(i).(3) *. u +. 2.0 *. c.(i).(2)) *. u +. c.(i).(1);
        ansy1 := u *. !ansy1 +. (3.0 *. c.(3).(i) *. t +. 2.0 *. c.(2).(i)) *. t +. c.(1).(i)
      done;
      ansy1 := !ansy1 /. d1;
      ansy2 := !ansy2 /. d2;
      print_string (fmt6 !ansy);
      print_string " ";
      print_string (fmt6 !ansy1);
      print_string " ";
      print_string (fmt6 !ansy2);
      print_newline ()
    done;
    exit 0);
  let m = int_of_string (next ()) in
  let n = int_of_string (next ()) in
  let x1v = Array.make m 0.0 and x2v = Array.make n 0.0 in
  for i = 0 to m - 1 do
    x1v.(i) <- float_of_string (next ())
  done;
  for j = 0 to n - 1 do
    x2v.(j) <- float_of_string (next ())
  done;
  let ym = Array.make_matrix m n 0.0 in
  for i = 0 to m - 1 do
    for j = 0 to n - 1 do
      ym.(i).(j) <- float_of_string (next ())
    done
  done;
  let mm = ref 0 and nn = ref 0 in
  if op = "POLY2D" then (
    mm := int_of_string (next ());
    nn := int_of_string (next ()));
  let q = int_of_string (next ()) in
  let rowy2 = Array.make m [||] in
  if op = "SPLINE2D" then
    for i = 0 to m - 1 do
      rowy2.(i) <- spline_y2 x2v ym.(i)
    done;
  for _qq = 0 to q - 1 do
    let x1p = float_of_string (next ()) in
    let x2p = float_of_string (next ()) in
    let ans = ref 0.0 in
    if op = "BILINEAR" then (
      let i = locate x1v x1p and j = locate x2v x2p in
      let t = (x1p -. x1v.(i)) /. (x1v.(i + 1) -. x1v.(i)) in
      let u = (x2p -. x2v.(j)) /. (x2v.(j + 1) -. x2v.(j)) in
      ans :=
        (1.0 -. t) *. (1.0 -. u) *. ym.(i).(j)
        +. t *. (1.0 -. u) *. ym.(i + 1).(j)
        +. (1.0 -. t) *. u *. ym.(i).(j + 1)
        +. t *. u *. ym.(i + 1).(j + 1))
    else if op = "POLY2D" then (
      let bi = ref 0 and bj = ref 0 in
      (let klo = ref 0 and khi = ref (m - 1) in
       while !khi - !klo > 1 do
         let k = (!khi + !klo) / 2 in
         if x1v.(k) > x1p then khi := k else klo := k
       done;
       bi := !klo - ((!mm - 2) / 2);
       if !bi < 0 then bi := 0;
       if !bi > m - !mm then bi := m - !mm);
      (let klo = ref 0 and khi = ref (n - 1) in
       while !khi - !klo > 1 do
         let k = (!khi + !klo) / 2 in
         if x2v.(k) > x2p then khi := k else klo := k
       done;
       bj := !klo - ((!nn - 2) / 2);
       if !bj < 0 then bj := 0;
       if !bj > n - !nn then bj := n - !nn);
      let yv = Array.make m 0.0 in
      for k = !bi to !bi + !mm - 1 do
        let xs = Array.make !nn 0.0 and ys = Array.make !nn 0.0 in
        for j = 0 to !nn - 1 do
          xs.(j) <- x2v.(!bj + j);
          ys.(j) <- ym.(k).(!bj + j)
        done;
        yv.(k) <- neville xs ys !nn x2p
      done;
      let xs2 = Array.make !mm 0.0 and ys2 = Array.make !mm 0.0 in
      for k = 0 to !mm - 1 do
        xs2.(k) <- x1v.(!bi + k);
        ys2.(k) <- yv.(!bi + k)
      done;
      ans := neville xs2 ys2 !mm x1p)
    else (
      let yv = Array.make m 0.0 in
      for i = 0 to m - 1 do
        let jl = locate x2v x2p in
        let h = x2v.(jl + 1) -. x2v.(jl) in
        let a = (x2v.(jl + 1) -. x2p) /. h and b = (x2p -. x2v.(jl)) /. h in
        yv.(i) <-
          a *. ym.(i).(jl) +. b *. ym.(i).(jl + 1)
          +. ((a *. a *. a -. a) *. rowy2.(i).(jl)
             +. (b *. b *. b -. b) *. rowy2.(i).(jl + 1))
             *. (h *. h) /. 6.0
      done;
      let cy2 = spline_y2 x1v yv in
      let jl = locate x1v x1p in
      let h = x1v.(jl + 1) -. x1v.(jl) in
      let a = (x1v.(jl + 1) -. x1p) /. h and b = (x1p -. x1v.(jl)) /. h in
      ans :=
        a *. yv.(jl) +. b *. yv.(jl + 1)
        +. ((a *. a *. a -. a) *. cy2.(jl)
           +. (b *. b *. b -. b) *. cy2.(jl + 1))
           *. (h *. h) /. 6.0);
    print_string (fmt6 !ans);
    print_newline ()
  done
