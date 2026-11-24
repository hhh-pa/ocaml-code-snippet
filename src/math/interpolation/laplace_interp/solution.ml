(* Laplace interpolation (NR 3.8). *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

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
  let nx = int_of_string (next ()) in
  let ny = int_of_string (next ()) in
  let x0 = float_of_string (next ()) in
  let x1 = float_of_string (next ()) in
  let y0 = float_of_string (next ()) in
  let y1 = float_of_string (next ()) in
  let k = int_of_string (next ()) in
  let px = Array.make k 0.0 and py = Array.make k 0.0 and pz = Array.make k 0.0 in
  for p = 0 to k - 1 do
    px.(p) <- float_of_string (next ());
    py.(p) <- float_of_string (next ());
    pz.(p) <- float_of_string (next ())
  done;
  let dx = (x1 -. x0) /. float (nx - 1) in
  let dy = (y1 -. y0) /. float (ny - 1) in
  let ci = Array.make k 0 and cj = Array.make k 0 in
  let w = Array.make_matrix k 4 0.0 in
  for p = 0 to k - 1 do
    let fx = ref ((px.(p) -. x0) /. dx) in
    let fy = ref ((py.(p) -. y0) /. dy) in
    if !fx < 0.0 then fx := 0.0;
    if !fy < 0.0 then fy := 0.0;
    if !fx > float (nx - 1) then fx := float (nx - 1);
    if !fy > float (ny - 1) then fy := float (ny - 1);
    let i = ref (int_of_float !fx) and j = ref (int_of_float !fy) in
    if !i > nx - 2 then i := nx - 2;
    if !j > ny - 2 then j := ny - 2;
    let tx = ref (!fx -. float !i) and ty = ref (!fy -. float !j) in
    if !tx < 0.0 then tx := 0.0;
    if !ty < 0.0 then ty := 0.0;
    if !tx > 1.0 then tx := 1.0;
    if !ty > 1.0 then ty := 1.0;
    ci.(p) <- !i; cj.(p) <- !j;
    w.(p).(0) <- (1.0 -. !tx) *. (1.0 -. !ty);
    w.(p).(1) <- !tx *. (1.0 -. !ty);
    w.(p).(2) <- (1.0 -. !tx) *. !ty;
    w.(p).(3) <- !tx *. !ty
  done;
  let ng = nx * ny in
  let n = ng + k in
  let idx i j = i * ny + j in
  let diag = Array.make n 0.0 in
  for i = 0 to nx - 1 do
    for j = 0 to ny - 1 do
      let d = ref 0 in
      if i > 0 then incr d;
      if i < nx - 1 then incr d;
      if j > 0 then incr d;
      if j < ny - 1 then incr d;
      diag.(idx i j) <- float !d
    done
  done;
  for p = 0 to k - 1 do
    diag.(ng + p) <- 1.0
  done;
  let matvec x y =
    for t = 0 to n - 1 do
      y.(t) <- 0.0
    done;
    for i = 0 to nx - 1 do
      for j = 0 to ny - 1 do
        let s = ref (diag.(idx i j) *. x.(idx i j)) in
        if i > 0 then s := !s -. x.(idx (i - 1) j);
        if i < nx - 1 then s := !s -. x.(idx (i + 1) j);
        if j > 0 then s := !s -. x.(idx i (j - 1));
        if j < ny - 1 then s := !s -. x.(idx i (j + 1));
        y.(idx i j) <- !s
      done
    done;
    for p = 0 to k - 1 do
      let i = ci.(p) and j = cj.(p) in
      let lam = x.(ng + p) in
      y.(idx i j) <- y.(idx i j) +. w.(p).(0) *. lam;
      y.(idx (i + 1) j) <- y.(idx (i + 1) j) +. w.(p).(1) *. lam;
      y.(idx i (j + 1)) <- y.(idx i (j + 1)) +. w.(p).(2) *. lam;
      y.(idx (i + 1) (j + 1)) <- y.(idx (i + 1) (j + 1)) +. w.(p).(3) *. lam;
      y.(ng + p) <-
        w.(p).(0) *. x.(idx i j) +. w.(p).(1) *. x.(idx (i + 1) j)
        +. w.(p).(2) *. x.(idx i (j + 1))
        +. w.(p).(3) *. x.(idx (i + 1) (j + 1))
    done
  in
  let dot a b =
    let s = ref 0.0 in
    for t = 0 to n - 1 do
      s := !s +. a.(t) *. b.(t)
    done;
    !s
  in
  let x = Array.make n 0.0 in
  let r = Array.make n 0.0 in
  let rt = Array.make n 0.0 in
  let pp = Array.make n 0.0 in
  let pt = Array.make n 0.0 in
  let z = Array.make n 0.0 in
  let zt = Array.make n 0.0 in
  let q = Array.make n 0.0 in
  let qt = Array.make n 0.0 in
  for p = 0 to k - 1 do
    x.(ng + p) <- pz.(p)
  done;
  matvec x q;
  for t = 0 to n - 1 do
    let b = if t < ng then 0.0 else pz.(t - ng) in
    r.(t) <- b -. q.(t)
  done;
  Array.blit r 0 rt 0 n;
  let rho = ref 1.0 in
  for _it = 0 to 399 do
    for t = 0 to n - 1 do
      z.(t) <- r.(t) /. diag.(t);
      zt.(t) <- rt.(t) /. diag.(t)
    done;
    let rhon = dot rt z in
    let beta = if !rho = 0.0 then 0.0 else rhon /. !rho in
    for t = 0 to n - 1 do
      pp.(t) <- z.(t) +. beta *. pp.(t);
      pt.(t) <- zt.(t) +. beta *. pt.(t)
    done;
    matvec pp q;
    matvec pt qt;
    let sg = dot pt q in
    let alpha = if sg = 0.0 then 0.0 else rhon /. sg in
    for t = 0 to n - 1 do
      x.(t) <- x.(t) +. alpha *. pp.(t);
      r.(t) <- r.(t) -. alpha *. q.(t);
      rt.(t) <- rt.(t) -. alpha *. qt.(t)
    done;
    rho := rhon
  done;
  for i = 0 to nx - 1 do
    for j = 0 to ny - 1 do
      if j > 0 then print_string " ";
      print_string (fmt6 x.(idx i j))
    done;
    print_newline ()
  done
