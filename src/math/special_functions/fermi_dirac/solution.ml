(* Generalized Fermi-Dirac integral (NR 6.10). *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

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
  if Array.length toks < 3 then exit 0;
  let kk = float_of_string toks.(0)
  and etaa = float_of_string toks.(1)
  and thetaa = float_of_string toks.(2) in
  let f_trap t =
    let e = exp (-.t) in
    let x = exp (t -. e) in
    x *. (1.0 +. e) *. (x ** kk) *. sqrt (1.0 +. thetaa *. 0.5 *. x)
    /. (exp (x -. etaa) +. 1.0)
  in
  let f_de x dl =
    let pw = if x < 1.0 then dl ** kk else x ** kk in
    pw *. sqrt (1.0 +. thetaa *. 0.5 *. x) /. (exp (x -. etaa) +. 1.0)
  in
  let eps = 3.0e-9 and nmax = 11 in
  let sum = ref 0.0 and olds = ref 0.0 and flag = ref 1 in
  if etaa <= 15.0 then (
    let a = -4.5 and b = 5.0 in
    let n = ref 0 and st = ref 0.0 in
    let i = ref 1 in
    while !i <= nmax && !flag = 1 do
      n := !n + 1;
      if !n = 1 then st := 0.5 *. (b -. a) *. (f_trap a +. f_trap b)
      else (
        let it = ref 1 in
        for _j = 1 to !n - 2 do
          it := !it * 2
        done;
        let tnm = float !it in
        let dl = (b -. a) /. tnm in
        let x = ref (a +. 0.5 *. dl) and acc = ref 0.0 in
        for _j = 0 to !it - 1 do
          acc := !acc +. f_trap !x;
          x := !x +. dl
        done;
        st := 0.5 *. (!st +. (b -. a) *. !acc /. tnm));
      sum := !st;
      if !i > 3 && abs_float (!sum -. !olds) <= eps *. abs_float !olds then flag := 0
      else olds := !sum;
      i := !i + 1
    done)
  else (
    let hmax = 4.3 in
    let aa = [| 0.0; etaa |] and bb = [| etaa; etaa +. 60.0 |] in
    let st = [| 0.0; 0.0 |] and nn = [| 0; 0 |] in
    let i = ref 1 in
    while !i <= nmax && !flag = 1 do
      sum := 0.0;
      for r = 0 to 1 do
        nn.(r) <- nn.(r) + 1;
        if nn.(r) = 1 then
          st.(r) <-
            hmax *. 2.0 *. (bb.(r) -. aa.(r)) *. 0.25
            *. f_de (0.5 *. (bb.(r) +. aa.(r))) (0.5 *. (bb.(r) -. aa.(r)))
        else (
          let it = ref 1 in
          for _j = 1 to nn.(r) - 2 do
            it := !it * 2
          done;
          let twoh = hmax /. float !it in
          let t = ref (0.5 *. twoh) and acc = ref 0.0 in
          for _j = 0 to !it - 1 do
            let q = exp (-2.0 *. sinh !t) in
            let dl = (bb.(r) -. aa.(r)) *. q /. (1.0 +. q) in
            let fact = q /. ((1.0 +. q) *. (1.0 +. q)) *. cosh !t in
            acc :=
              !acc +. fact *. (f_de (aa.(r) +. dl) dl +. f_de (bb.(r) -. dl) dl);
            t := !t +. twoh
          done;
          st.(r) <- 0.5 *. st.(r) +. (bb.(r) -. aa.(r)) *. twoh *. !acc);
        sum := !sum +. st.(r)
      done;
      if !i > 3 && abs_float (!sum -. !olds) <= eps *. abs_float !olds then flag := 0
      else olds := !sum;
      i := !i + 1
    done);
  Printf.printf "%s %d\n" (fmt6 (canon !sum)) !flag
