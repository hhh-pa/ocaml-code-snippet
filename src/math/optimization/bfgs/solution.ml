(* Multidimensional minimization by the Broyden-Fletcher-
   Goldfarb-Shanno quasi-Newton method (NR dfpmin, BFGS
   updating, lnsrch backtracking line searches).
   Reference: Press et al., Numerical Recipes 3e, Section 10.9.
   No online-judge problem found -- fixtures are self-generated
   (scipy oracle), no header link. *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

let fn = ref ""
let fp = ref [||]

let func x =
  if !fn = "QUAD" then (
    let n = Array.length x in
    let s = ref !fp.(n) in
    for i = 0 to n - 1 do
      s := !s +. (x.(i) -. !fp.(i)) *. (x.(i) -. !fp.(i))
    done;
    !s)
  else (
    let n = Array.length x in
    let s = ref 0.0 in
    for i = 0 to n - 2 do
      let a = x.(i + 1) -. x.(i) *. x.(i) in
      let b = 1.0 -. x.(i) in
      s := !s +. 100.0 *. a *. a +. b *. b
    done;
    !s)

let funcdf x df =
  let n = Array.length x in
  if !fn = "QUAD" then
    for i = 0 to n - 1 do
      df.(i) <- 2.0 *. (x.(i) -. !fp.(i))
    done
  else (
    df.(0) <- -400.0 *. x.(0) *. (x.(1) -. x.(0) *. x.(0)) -. 2.0 *. (1.0 -. x.(0));
    for i = 1 to n - 2 do
      df.(i) <-
        200.0 *. (x.(i) -. x.(i - 1) *. x.(i - 1))
        -. 400.0 *. x.(i) *. (x.(i + 1) -. x.(i) *. x.(i))
        -. 2.0 *. (1.0 -. x.(i))
    done;
    df.(n - 1) <- 200.0 *. (x.(n - 1) -. x.(n - 2) *. x.(n - 2)))

exception TooMany

let lnsrch xold fold g p x stpmax f_in =
  let alf = 1.0e-4 and tolx = 2.220446049250313e-16 in
  let n = Array.length xold in
  let sum = ref 0.0 in
  for i = 0 to n - 1 do
    sum := !sum +. p.(i) *. p.(i)
  done;
  sum := sqrt !sum;
  if !sum > stpmax then
    for i = 0 to n - 1 do
      p.(i) <- p.(i) *. (stpmax /. !sum)
    done;
  let slope = ref 0.0 in
  for i = 0 to n - 1 do
    slope := !slope +. g.(i) *. p.(i)
  done;
  if !slope >= 0.0 then (f_in, false, 1)
  else (
    let test = ref 0.0 in
    for i = 0 to n - 1 do
      let temp = abs_float p.(i) /. max (abs_float xold.(i)) 1.0 in
      if temp > !test then test := temp
    done;
    let alamin = tolx /. !test in
    let alam = ref 1.0 and alam2 = ref 0.0 and f2 = ref 0.0 in
    let res = ref None in
    while !res = None do
      for i = 0 to n - 1 do
        x.(i) <- xold.(i) +. !alam *. p.(i)
      done;
      let f = func x in
      if !alam < alamin then (
        Array.blit xold 0 x 0 n;
        res := Some (f, true, 0))
      else if f <= fold +. alf *. !alam *. !slope then res := Some (f, false, 0)
      else (
        let tmplam =
          if !alam = 1.0 then (-. !slope) /. (2.0 *. (f -. fold -. !slope))
          else (
            let rhs1 = f -. fold -. !alam *. !slope in
            let rhs2 = !f2 -. fold -. !alam2 *. !slope in
            let a =
              (rhs1 /. (!alam *. !alam) -. rhs2 /. (!alam2 *. !alam2)) /. (!alam -. !alam2)
            in
            let b =
              ((-. !alam2 *. rhs1 /. (!alam *. !alam))
               +. !alam *. rhs2 /. (!alam2 *. !alam2))
              /. (!alam -. !alam2)
            in
            let raw =
              if a = 0.0 then (-. !slope) /. (2.0 *. b)
              else (
                let disc = b *. b -. 3.0 *. a *. !slope in
                if disc < 0.0 then 0.5 *. !alam
                else if b <= 0.0 then (-. b +. sqrt disc) /. (3.0 *. a)
                else (-. !slope) /. (b +. sqrt disc))
            in
            if raw > 0.5 *. !alam then 0.5 *. !alam else raw)
        in
        alam2 := !alam;
        f2 := f;
        alam := max tmplam (0.1 *. !alam))
    done;
    match !res with Some v -> v | None -> assert false)

let minimize point gtol =
  let itmax = 200 in
  let eps = 2.220446049250313e-16 in
  let tolx = 4.0 *. eps and stpmx = 100.0 in
  let n = Array.length point in
  let p = Array.copy point in
  let dg = Array.make n 0.0
  and g = Array.make n 0.0
  and hdg = Array.make n 0.0
  and pnew = Array.make n 0.0
  and xi = Array.make n 0.0 in
  let hessin = Array.make_matrix n n 0.0 in
  let fret = ref 0.0 and iter = ref 0 in
  funcdf p g;
  let sum = ref 0.0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      hessin.(i).(j) <- 0.0
    done;
    hessin.(i).(i) <- 1.0;
    xi.(i) <- -. g.(i);
    sum := !sum +. p.(i) *. p.(i)
  done;
  let stpmax = stpmx *. max (sqrt !sum) (float n) in
  let rec loop it fp =
    if it >= itmax then raise TooMany
    else (
      iter := it;
      Array.blit p 0 pnew 0 n;
      let f, _, _ = lnsrch p fp g xi pnew stpmax !fret in
      fret := f;
      for i = 0 to n - 1 do
        xi.(i) <- pnew.(i) -. p.(i);
        p.(i) <- pnew.(i)
      done;
      let test = ref 0.0 in
      for i = 0 to n - 1 do
        let temp = abs_float xi.(i) /. max (abs_float p.(i)) 1.0 in
        if temp > !test then test := temp
      done;
      if !test < tolx then (p, f, it)
      else (
        for i = 0 to n - 1 do
          dg.(i) <- g.(i)
        done;
        funcdf p g;
        let test2 = ref 0.0 in
        let den = max f 1.0 in
        for i = 0 to n - 1 do
          let temp = abs_float g.(i) *. max (abs_float p.(i)) 1.0 /. den in
          if temp > !test2 then test2 := temp
        done;
        if !test2 < gtol then (p, f, it)
        else (
          for i = 0 to n - 1 do
            dg.(i) <- g.(i) -. dg.(i)
          done;
          for i = 0 to n - 1 do
            hdg.(i) <- 0.0;
            for j = 0 to n - 1 do
              hdg.(i) <- hdg.(i) +. hessin.(i).(j) *. dg.(j)
            done
          done;
          let fac = ref 0.0 and fae = ref 0.0 and sumdg = ref 0.0 and sumxi = ref 0.0 in
          for i = 0 to n - 1 do
            fac := !fac +. dg.(i) *. xi.(i);
            fae := !fae +. dg.(i) *. hdg.(i);
            sumdg := !sumdg +. dg.(i) *. dg.(i);
            sumxi := !sumxi +. xi.(i) *. xi.(i)
          done;
          (if !fac > sqrt (eps *. !sumdg *. !sumxi) then (
             let rf = 1.0 /. !fac and rd = 1.0 /. !fae in
             for i = 0 to n - 1 do
               dg.(i) <- rf *. xi.(i) -. rd *. hdg.(i)
             done;
             for i = 0 to n - 1 do
               for j = i to n - 1 do
                 hessin.(i).(j) <-
                   hessin.(i).(j)
                   +. rf *. xi.(i) *. xi.(j)
                   -. rd *. hdg.(i) *. hdg.(j)
                   +. !fae *. dg.(i) *. dg.(j);
                 hessin.(j).(i) <- hessin.(i).(j)
               done
             done));
          for i = 0 to n - 1 do
            xi.(i) <- 0.0;
            for j = 0 to n - 1 do
              xi.(i) <- xi.(i) -. hessin.(i).(j) *. g.(j)
            done
          done;
          loop (it + 1) f)))
  in
  try
    let pmin, fr, it = loop 0 (func p) in
    (pmin, fr, it, 0)
  with TooMany -> (p, !fret, !iter, 1)

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
  fn := toks.(1);
  let n = int_of_string toks.(2) in
  let p = ref 3 in
  (if !fn = "QUAD" then (
     fp := Array.init (n + 1) (fun i -> float_of_string toks.(3 + i));
     p := 3 + n + 1));
  let point = Array.init n (fun i -> float_of_string toks.(!p + i)) in
  let gtol = float_of_string toks.(!p + n) in
  if op = "BFGS" then (
    let pmin, fret, it, flag = minimize point gtol in
    if flag <> 0 then (
      let parts = Array.to_list (Array.init n (fun _ -> "0.000000")) in
      print_endline (String.concat " " parts ^ " 0.000000 " ^ string_of_int it ^ " 1"))
    else (
      let parts = Array.to_list (Array.init n (fun i -> fmt6 (canon pmin.(i)))) in
      print_endline
        (String.concat " " parts ^ " " ^ fmt6 (canon fret) ^ " " ^ string_of_int it ^ " 0")))
