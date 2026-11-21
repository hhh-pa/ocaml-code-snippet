(* Rational Chebyshev approximation (NR 5.13, ratlsq). Port of the C++ reference. *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v
let fmtv v = fmt6 (canon v)
let pi = 3.141592653589793

let svdsolve a b =
  let m = Array.length a in
  let n = Array.length a.(0) in
  let bb = Array.map Array.copy a in
  let v = Array.make_matrix n n 0.0 in
  for i = 0 to n - 1 do
    v.(i).(i) <- 1.0
  done;
  for _sw = 0 to 11 do
    for p = 0 to n - 2 do
      for q = p + 1 to n - 1 do
        let alpha = ref 0.0 and gamma = ref 0.0 and beta = ref 0.0 in
        for k = 0 to m - 1 do
          alpha := !alpha +. bb.(k).(p) *. bb.(k).(p);
          gamma := !gamma +. bb.(k).(q) *. bb.(k).(q);
          beta := !beta +. bb.(k).(p) *. bb.(k).(q)
        done;
        if !beta <> 0.0 then (
          let zeta = (!gamma -. !alpha) /. (2.0 *. !beta) in
          let t =
            (if zeta < 0.0 then -1.0 else 1.0)
            /. (abs_float zeta +. sqrt (1.0 +. zeta *. zeta))
          in
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
          done)
      done
    done
  done;
  let w = Array.make n 0.0 in
  let u = Array.make_matrix m n 0.0 in
  for j = 0 to n - 1 do
    let s = ref 0.0 in
    for k = 0 to m - 1 do
      s := !s +. bb.(k).(j) *. bb.(k).(j)
    done;
    w.(j) <- sqrt !s;
    if w.(j) <> 0.0 then
      for k = 0 to m - 1 do
        u.(k).(j) <- bb.(k).(j) /. w.(j)
      done
  done;
  let wmax = ref 0.0 in
  for j = 0 to n - 1 do
    if w.(j) > !wmax then wmax := w.(j)
  done;
  let thresh = 1.0e-13 *. !wmax in
  let tmp = Array.make n 0.0 in
  for j = 0 to n - 1 do
    let s = ref 0.0 in
    if w.(j) > thresh then (
      for i = 0 to m - 1 do
        s := !s +. u.(i).(j) *. b.(i)
      done;
      s := !s /. w.(j));
    tmp.(j) <- !s
  done;
  let x = Array.make n 0.0 in
  for j = 0 to n - 1 do
    let s = ref 0.0 in
    for jj = 0 to n - 1 do
      s := !s +. v.(j).(jj) *. tmp.(jj)
    done;
    x.(j) <- !s
  done;
  x

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
  let fun_ = next () in
  let p =
    if fun_ = "POLY" then (
      let d = int_of_string (next ()) in
      Array.init (d + 1) (fun _ -> float_of_string (next ())))
    else Array.init 3 (fun _ -> float_of_string (next ()))
  in
  let a = float_of_string (next ()) in
  let b = float_of_string (next ()) in
  let mm = int_of_string (next ()) in
  let kk = int_of_string (next ()) in
  let f t =
    if fun_ = "POLY" then (
      let s = ref 0.0 in
      for i = Array.length p - 1 downto 0 do
        s := !s *. t +. p.(i)
      done;
      !s)
    else if fun_ = "EXP" then p.(0) *. exp (p.(1) *. t) +. p.(2)
    else if fun_ = "TRIG" then p.(0) *. sin (p.(1) *. t +. p.(2))
    else p.(0) *. log (p.(1) *. t +. p.(2))
  in
  let pio2 = pi /. 2.0 in
  let ncof = mm + kk + 1 in
  let npt = 8 * ncof in
  let bbv = Array.make npt 0.0 in
  let ee = Array.make npt 1.0 in
  let fs = Array.make npt 0.0 in
  let wt = Array.make npt 1.0 in
  let xs = Array.make npt 0.0 in
  let u = Array.make_matrix npt ncof 0.0 in
  let best = Array.make ncof 0.0 in
  let dev = ref 1.0e99 in
  for i = 0 to npt - 1 do
    (if i < npt / 2 - 1 then (
       let hth = pio2 *. float i /. float (npt - 1) in
       let sn = sin hth in
       xs.(i) <- a +. (b -. a) *. sn *. sn)
     else (
       let hth = pio2 *. float (npt - i) /. float (npt - 1) in
       let sn = sin hth in
       xs.(i) <- b -. (b -. a) *. sn *. sn));
    fs.(i) <- f xs.(i)
  done;
  let e = ref 0.0 in
  for _it = 0 to 4 do
    for i = 0 to npt - 1 do
      let power0 = wt.(i) in
      let se = ref (abs_float !e) in
      if ee.(i) < 0.0 then se := -. !se;
      bbv.(i) <- power0 *. (fs.(i) +. !se);
      let power = ref power0 in
      for j = 0 to mm do
        u.(i).(j) <- !power;
        power := !power *. xs.(i)
      done;
      power := -.bbv.(i);
      for j = mm + 1 to ncof - 1 do
        power := !power *. xs.(i);
        u.(i).(j) <- !power
      done
    done;
    let coff = svdsolve u bbv in
    let devmax = ref 0.0 and sum = ref 0.0 in
    for j = 0 to npt - 1 do
      let num = ref 0.0 and den = ref 0.0 in
      for t = mm downto 0 do
        num := !num *. xs.(j) +. coff.(t)
      done;
      for t = kk downto 1 do
        den := !den *. xs.(j) +. coff.(mm + t)
      done;
      den := !den *. xs.(j) +. 1.0;
      ee.(j) <- !num /. !den -. fs.(j);
      wt.(j) <- abs_float ee.(j);
      sum := !sum +. wt.(j);
      if wt.(j) > !devmax then devmax := wt.(j)
    done;
    e := !sum /. float npt;
    if !devmax <= !dev then (
      dev := !devmax;
      Array.blit coff 0 best 0 ncof)
  done;
  for j = 0 to mm do
    if j > 0 then print_string " ";
    print_string (fmtv best.(j))
  done;
  print_newline ();
  for j = 1 to kk do
    if j > 1 then print_string " ";
    print_string (fmtv best.(mm + j))
  done;
  print_newline ();
  print_endline (fmt6 (!dev +. 0.0))
