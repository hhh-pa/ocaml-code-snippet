(* Polynomial roots (NR 9.5). *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

type cx = { re : float; im : float }

let cxadd a b = { re = a.re +. b.re; im = a.im +. b.im }
let cxsub a b = { re = a.re -. b.re; im = a.im -. b.im }
let cxmul a b =
  { re = a.re *. b.re -. a.im *. b.im; im = a.re *. b.im +. a.im *. b.re }

let cxdiv a b =
  let d = b.re *. b.re +. b.im *. b.im in
  { re = (a.re *. b.re +. a.im *. b.im) /. d;
    im = (a.im *. b.re -. a.re *. b.im) /. d }

let cxscale a s = { re = a.re *. s; im = a.im *. s }
let cxabs a = sqrt (a.re *. a.re +. a.im *. a.im)

let cxsqrt w =
  if w.im = 0.0 then
    if w.re >= 0.0 then { re = sqrt w.re; im = 0.0 }
    else { re = 0.0; im = sqrt (-.w.re) }
  else (
    let r = cxabs w in
    let re = sqrt ((r +. w.re) *. 0.5) and im = sqrt ((r -. w.re) *. 0.5) in
    { re; im = (if w.im >= 0.0 then im else -.im) })

let cxpolar r th = { re = r *. cos th; im = r *. sin th }
let maceps = 2.220446049250313e-16
let frac = [| 0.0; 0.5; 0.25; 0.75; 0.13; 0.38; 0.62; 0.88; 1.0 |]

let laguer a x0 =
  let mt = 10 and maxit = 80 in
  let m = Array.length a - 1 in
  let x = ref x0 and its = ref 0 and res = ref None in
  for iter = 1 to maxit do
    if !res = None then (
      its := iter;
      let b = ref a.(m) in
      let err = ref (cxabs !b) in
      let d = ref { re = 0.0; im = 0.0 } and f = ref { re = 0.0; im = 0.0 } in
      let abx = cxabs !x in
      for j = m - 1 downto 0 do
        f := cxadd (cxmul !x !f) !d;
        d := cxadd (cxmul !x !d) !b;
        b := cxadd (cxmul !x !b) a.(j);
        err := cxabs !b +. abx *. !err
      done;
      err := !err *. maceps;
      if cxabs !b <= !err then res := Some (!x, !its, 0)
      else (
        let g = cxdiv !d !b in
        let g2 = cxmul g g in
        let h = cxsub g2 (cxscale (cxdiv !f !b) 2.0) in
        let sq = cxsqrt (cxscale (cxsub (cxscale h (float m)) g2) (float (m - 1))) in
        let gp0 = cxadd g sq and gm = cxsub g sq in
        let abp = cxabs gp0 and abm = cxabs gm in
        let gp = if abp < abm then gm else gp0 in
        let mx = if abp < abm then abm else abp in
        let dx =
          if mx > 0.0 then cxdiv { re = float m; im = 0.0 } gp
          else cxpolar (1.0 +. abx) (float iter)
        in
        let x1 = cxsub !x dx in
        if !x.re = x1.re && !x.im = x1.im then res := Some (!x, !its, 0)
        else if iter mod mt <> 0 then x := x1
        else x := cxsub !x (cxscale dx frac.(iter / mt))))
  done;
  (match !res with Some v -> v | None -> (!x, !its, 1))

let poldiv u v =
  let n = Array.length u - 1 in
  let nv = ref (Array.length v - 1) in
  while !nv >= 0 && v.(!nv) = 0.0 do
    nv := !nv - 1
  done;
  let r = Array.copy u and q = Array.make (Array.length u) 0.0 in
  let k = ref (n - !nv) in
  while !k >= 0 do
    q.(!k) <- r.(!nv + !k) /. v.(!nv);
    let j = ref (!nv + !k - 1) in
    while !j >= !k do
      r.(!j) <- r.(!j) -. q.(!k) *. v.(!j - !k);
      j := !j - 1
    done;
    k := !k - 1
  done;
  for j = !nv to n do
    r.(j) <- 0.0
  done;
  (q, r)

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
  let out = ref [] in
  let emit s = out := s :: !out in
  (if op = "ZROOTS" then (
     let m = int_of_string toks.(1) in
     let a =
       Array.init (m + 1) (fun i ->
           { re = float_of_string toks.(2 + (2 * i)); im = float_of_string toks.(3 + (2 * i)) })
     in
     let polish = int_of_string toks.(2 + (2 * (m + 1))) in
     let eps = 1.0e-14 in
     let ad = Array.copy a in
     let roots = Array.make m { re = 0.0; im = 0.0 } in
     let flag = ref 0 in
     let j = ref (m - 1) in
     while !j >= 0 && !flag = 0 do
       let adv = Array.sub ad 0 (!j + 2) in
       let x, _, fl = laguer adv { re = 0.0; im = 0.0 } in
       flag := fl;
       (if !flag = 0 then (
          let x =
            if abs_float x.im <= 2.0 *. eps *. abs_float x.re then { x with im = 0.0 } else x
          in
          roots.(!j) <- x;
          let b = ref ad.(!j + 1) in
          for jj = !j downto 0 do
            let c = ad.(jj) in
            ad.(jj) <- !b;
            b := cxadd (cxmul x !b) c
          done));
       j := !j - 1
     done;
     (if !flag = 0 && polish <> 0 then (
        let k = ref 0 in
        while !k < m && !flag = 0 do
          let x, _, fl = laguer a roots.(!k) in
          roots.(!k) <- x;
          flag := fl;
          k := !k + 1
        done));
     (if !flag = 0 then
        for k = 1 to m - 1 do
          let x = roots.(k) in
          let i = ref (k - 1) in
          while !i >= 0 && not (roots.(!i).re <= x.re) do
            roots.(!i + 1) <- roots.(!i);
            i := !i - 1
          done;
          roots.(!i + 1) <- x
        done);
     for k = 0 to m - 1 do
       (if !flag <> 0 then (
          emit (fmt6 (canon 0.0));
          emit (fmt6 (canon 0.0)))
        else (
          emit (fmt6 (canon roots.(k).re));
          emit (fmt6 (canon roots.(k).im))))
     done;
     emit (string_of_int !flag))
   else if op = "LAGUER" then (
     let m = int_of_string toks.(1) in
     let a =
       Array.init (m + 1) (fun i ->
           { re = float_of_string toks.(2 + (2 * i)); im = float_of_string toks.(3 + (2 * i)) })
     in
     let base = 2 + (2 * (m + 1)) in
     let x0 = { re = float_of_string toks.(base); im = float_of_string toks.(base + 1) } in
     let x, its, flag = laguer a x0 in
     if flag <> 0 then (
       emit "0.000000";
       emit "0.000000";
       emit "0";
       emit "1")
     else (
       emit (fmt6 (canon x.re));
       emit (fmt6 (canon x.im));
       emit (string_of_int its);
       emit "0"))
   else if op = "QROOT" then (
     let n = int_of_string toks.(1) in
     let p = Array.init (n + 1) (fun i -> float_of_string toks.(2 + i)) in
     let b = ref (float_of_string toks.(3 + n))
     and c = ref (float_of_string toks.(4 + n))
     and eps = float_of_string toks.(5 + n) in
     let tiny = 1.0e-14 in
     let d = [| 0.0; 0.0; 1.0 |] in
     let flag = ref 0 and it = ref 0 and cont = ref true in
     while !it < 20 && !cont do
       d.(1) <- !b;
       d.(0) <- !c;
       let q, rem = poldiv p d in
       let s = rem.(0) and r = rem.(1) in
       let _, rem2 = poldiv q d in
       let rc = -.rem2.(1) in
       let sb = -. !c *. rc in
       let sc = -.rem2.(0) in
       let rb = -. !b *. rc +. sc in
       let dv = 1.0 /. (sb *. rc -. sc *. rb) in
       let delb = (r *. sc -. s *. rc) *. dv in
       let delc = (-.r *. sb +. s *. rb) *. dv in
       b := !b +. delb;
       c := !c +. delc;
       (if
          (abs_float delb <= eps *. abs_float !b || abs_float !b < tiny)
          && (abs_float delc <= eps *. abs_float !c || abs_float !c < tiny)
        then cont := false
        else if !it = 19 then (
          flag := 1;
          cont := false));
       it := !it + 1
     done;
     if !flag = 1 then (
       emit "0.000000";
       emit "0.000000";
       emit "1")
     else (
       emit (fmt6 (canon !b));
       emit (fmt6 (canon !c));
       emit "0"))
   else exit 0);
  print_endline (String.concat " " (List.rev !out))
