(* Curve interpolation (NR 3.7.5). *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let frad a b dim =
  let s = ref 0.0 in
  for i = 0 to dim - 1 do
    s := !s +. (a.(i) -. b.(i)) *. (a.(i) -. b.(i))
  done;
  sqrt !s

let fprime x y pm =
  let n = Array.length x in
  let e = if pm = 1 then 0 else n - 1 in
  let s1 = x.(e) -. x.(e + pm * 1)
  and s2 = x.(e) -. x.(e + pm * 2)
  and s3 = x.(e) -. x.(e + pm * 3) in
  let s12 = s1 -. s2 and s13 = s1 -. s3 and s23 = s2 -. s3 in
  -.(s1 *. s2 /. (s13 *. s23 *. s3)) *. y.(e + pm * 3)
  +. (s1 *. s3 /. (s12 *. s2 *. s23)) *. y.(e + pm * 2)
  -. (s2 *. s3 /. (s1 *. s12 *. s13)) *. y.(e + pm * 1)
  +. (1.0 /. s1 +. 1.0 /. s2 +. 1.0 /. s3) *. y.(e)

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
  let mode = next () in
  let cls = mode = "CLOSED" in
  let n = int_of_string (next ()) in
  let dim = int_of_string (next ()) in
  let ptsin = Array.make_matrix n dim 0.0 in
  for i = 0 to n - 1 do
    for j = 0 to dim - 1 do
      ptsin.(i).(j) <- float_of_string (next ())
    done
  done;
  let inn = if cls then 2 * n else n in
  let ofs = if cls then n / 2 else 0 in
  let pts = Array.make_matrix dim inn 0.0 in
  let s = Array.make inn 0.0 in
  for i = 0 to inn - 1 do
    let ii = (i - ofs + n) mod n in
    let im = (ii - 1 + n) mod n in
    for j = 0 to dim - 1 do
      pts.(j).(i) <- ptsin.(ii).(j)
    done;
    if i > 0 then s.(i) <- s.(i - 1) +. frad ptsin.(ii) ptsin.(im) dim
  done;
  let ss = if cls then s.(ofs + n) -. s.(ofs) else s.(n - 1) -. s.(0) in
  let soff = s.(ofs) in
  for i = 0 to inn - 1 do
    s.(i) <- (s.(i) -. soff) /. ss
  done;
  let nn = inn in
  let y2 = Array.make_matrix dim nn 0.0 in
  for j = 0 to dim - 1 do
    let db = if inn < 4 then 1.0e99 else fprime s pts.(j) 1 in
    let de = if inn < 4 then 1.0e99 else fprime s pts.(j) (-1) in
    let u = Array.make (nn - 1) 0.0 in
    let yv = pts.(j) and yy2 = y2.(j) in
    if db > 0.99e99 then (
      yy2.(0) <- 0.0;
      u.(0) <- 0.0)
    else (
      yy2.(0) <- -0.5;
      u.(0) <-
        (3.0 /. (s.(1) -. s.(0)))
        *. ((yv.(1) -. yv.(0)) /. (s.(1) -. s.(0)) -. db));
    for i = 1 to nn - 2 do
      let sg = (s.(i) -. s.(i - 1)) /. (s.(i + 1) -. s.(i - 1)) in
      let p = sg *. yy2.(i - 1) +. 2.0 in
      yy2.(i) <- (sg -. 1.0) /. p;
      let ui =
        (yv.(i + 1) -. yv.(i)) /. (s.(i + 1) -. s.(i))
        -. (yv.(i) -. yv.(i - 1)) /. (s.(i) -. s.(i - 1))
      in
      u.(i) <- (6.0 *. ui /. (s.(i + 1) -. s.(i - 1)) -. sg *. u.(i - 1)) /. p
    done;
    let qn, un =
      if de > 0.99e99 then (0.0, 0.0)
      else
        ( 0.5,
          (3.0 /. (s.(nn - 1) -. s.(nn - 2)))
          *. (de -. (yv.(nn - 1) -. yv.(nn - 2)) /. (s.(nn - 1) -. s.(nn - 2))) )
    in
    yy2.(nn - 1) <- (un -. qn *. u.(nn - 2)) /. (qn *. yy2.(nn - 2) +. 1.0);
    for k = nn - 2 downto 0 do
      yy2.(k) <- yy2.(k) *. yy2.(k + 1) +. u.(k)
    done
  done;
  let q = int_of_string (next ()) in
  for _qq = 0 to q - 1 do
    let t = ref (float_of_string (next ())) in
    if cls then t := !t -. floor !t;
    for j = 0 to dim - 1 do
      let klo = ref 0 and khi = ref (nn - 1) in
      while !khi - !klo > 1 do
        let k = (!khi + !klo) / 2 in
        if s.(k) > !t then khi := k else klo := k
      done;
      let jl = ref !klo in
      if !jl < 0 then jl := 0;
      if !jl > nn - 2 then jl := nn - 2;
      let h = s.(!jl + 1) -. s.(!jl) in
      let a = (s.(!jl + 1) -. !t) /. h and b = (!t -. s.(!jl)) /. h in
      let y =
        a *. pts.(j).(!jl) +. b *. pts.(j).(!jl + 1)
        +. ((a *. a *. a -. a) *. y2.(j).(!jl)
           +. (b *. b *. b -. b) *. y2.(j).(!jl + 1))
           *. (h *. h) /. 6.0
      in
      if j > 0 then print_string " ";
      print_string (fmt6 y)
    done;
    print_newline ()
  done
