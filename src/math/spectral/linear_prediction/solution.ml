(* Linear prediction: Burg LP coefficients (memcof), stabilization by
   root reflection (fixrts), and extrapolation (predic).
   Reference: Press et al., Numerical Recipes 3e, Section 13.7
   (memcof/fixrts/predic, vendoring zroots+laguer from 9.5 with the
   explicit real/imag complex flops of math/roots/poly_roots).
   Documented decisions: n >= 2 and 1 <= m < n are enforced (the
   book indexes out of bounds for n = 1 and divides 0/0 for
   m >= n); an exact-zero denom flags (the book yields NaN);
   PREDIC needs ndata >= m >= 1 (the book overruns or
   dereferences an empty register otherwise).
   No online-judge problem found -- fixtures are self-generated
   (numpy oracle), no header link. *)

type cx = { re : float; im : float }

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

(* NOTE: float multiplies feeding +. or -. are let-bound first:
   ocamlopt fuses a direct mul-add/sub nest into FMA, which rounds
   once instead of twice and breaks bit-agreement with the C++
   reference (verified: zero FMA in the object code). *)

exception BadN

let cxadd a b = { re = a.re +. b.re; im = a.im +. b.im }
let cxsub a b = { re = a.re -. b.re; im = a.im -. b.im }

let cxmul a b =
  let p1 = a.re *. b.re in
  let p2 = a.im *. b.im in
  let p3 = a.re *. b.im in
  let p4 = a.im *. b.re in
  { re = p1 -. p2; im = p3 +. p4 }

let cxdiv a b =
  let s1 = b.re *. b.re in
  let s2 = b.im *. b.im in
  let d = s1 +. s2 in
  let n1 = a.re *. b.re in
  let n2 = a.im *. b.im in
  let n3 = a.im *. b.re in
  let n4 = a.re *. b.im in
  { re = (n1 +. n2) /. d; im = (n3 -. n4) /. d }

let cxscale a s = { re = a.re *. s; im = a.im *. s }

let cxabs a =
  let s1 = a.re *. a.re in
  let s2 = a.im *. a.im in
  sqrt (s1 +. s2)

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
  let m = Array.length a - 1 in
  let x = ref x0 and its = ref 0 and res = ref None in
  (try
     for iter = 1 to 80 do
       its := iter;
       let b = ref a.(m) in
       let err = ref (cxabs !b) in
       let d = ref { re = 0.0; im = 0.0 } and f = ref { re = 0.0; im = 0.0 } in
       let abx = cxabs !x in
       for j = m - 1 downto 0 do
         f := cxadd (cxmul !x !f) !d;
         d := cxadd (cxmul !x !d) !b;
         b := cxadd (cxmul !x !b) a.(j);
         let t = abx *. !err in
         err := cxabs !b +. t
       done;
       err := !err *. maceps;
       if cxabs !b <= !err then (
         res := Some (!x, !its, 0);
         raise Exit);
       let g = cxdiv !d !b in
       let g2 = cxmul g g in
       let h = cxsub g2 (cxscale (cxdiv !f !b) 2.0) in
       let sq = cxsqrt (cxscale (cxsub (cxscale h (float m)) g2) (float (m - 1))) in
       let gp0 = cxadd g sq in
       let gm = cxsub g sq in
       let abp = cxabs gp0 and abm = cxabs gm in
       let gp = if abp < abm then gm else gp0 in
       let mx = if abp < abm then abm else abp in
       let dx =
         if mx > 0.0 then cxdiv { re = float m; im = 0.0 } gp
         else cxpolar (1.0 +. abx) (float iter)
       in
       let x1 = cxsub !x dx in
       if !x.re = x1.re && !x.im = x1.im then (
         res := Some (!x, !its, 0);
         raise Exit);
       if iter mod 10 <> 0 then x := x1
       else x := cxsub !x (cxscale dx frac.(iter / 10))
     done
   with Exit -> ());
  match !res with
  | Some r -> r
  | None -> (!x, !its, 1)

let zroots a =
  let m = Array.length a - 1 in
  let ad = Array.copy a in
  let roots = Array.make m { re = 0.0; im = 0.0 } in
  let flag = ref 0 in
  let j = ref (m - 1) in
  while !j >= 0 && !flag = 0 do
    let adv = Array.sub ad 0 (!j + 2) in
    let x, _, fl = laguer adv { re = 0.0; im = 0.0 } in
    if fl <> 0 then flag := 1
    else (
      let x =
        if abs_float x.im <= 2.0 *. 1.0e-14 *. abs_float x.re then { x with im = 0.0 } else x
      in
      roots.(!j) <- x;
      let b = ref ad.(!j + 1) in
      for jj = !j downto 0 do
        let c = ad.(jj) in
        ad.(jj) <- !b;
        b := cxadd (cxmul x !b) c
      done);
    decr j
  done;
  let j = ref 0 in
  while !j < m && !flag = 0 do
    let x, _, fl = laguer a roots.(!j) in
    if fl <> 0 then flag := 1 else roots.(!j) <- x;
    incr j
  done;
  if !flag = 0 then
    for j = 1 to m - 1 do
      let x = roots.(j) in
      let i = ref (j - 1) in
      while !i >= 0 && not (roots.(!i).re <= x.re) do
        roots.(!i + 1) <- roots.(!i);
        decr i
      done;
      roots.(!i + 1) <- x
    done;
  (roots, !flag)

let memcof data m =
  let n = Array.length data in
  if n < 2 || m < 1 || m >= n then raise BadN;
  let d = Array.make m 0.0 in
  let p = ref 0.0 in
  let wk1 = Array.make n 0.0 and wk2 = Array.make n 0.0 and wkm = Array.make m 0.0 in
  for j = 0 to n - 1 do
    let t = data.(j) *. data.(j) in
    p := !p +. t
  done;
  let xms = ref (!p /. float_of_int n) in
  wk1.(0) <- data.(0);
  wk2.(n - 2) <- data.(n - 1);
  for j = 1 to n - 2 do
    wk1.(j) <- data.(j);
    wk2.(j - 1) <- data.(j)
  done;
  (try
     for k = 0 to m - 1 do
       let num = ref 0.0 and denom = ref 0.0 in
       for j = 0 to n - k - 2 do
         let t1 = wk1.(j) *. wk2.(j) in
         num := !num +. t1;
         let s1 = wk1.(j) *. wk1.(j) in
         let s2 = wk2.(j) *. wk2.(j) in
         denom := !denom +. s1 +. s2
       done;
       if !denom = 0.0 then raise BadN;
       d.(k) <- 2.0 *. !num /. !denom;
       let t = d.(k) in
       let tt = t *. t in
       xms := !xms *. (1.0 -. tt);
       for i = 0 to k - 1 do
         let u = d.(k) *. wkm.(k - 1 - i) in
         d.(i) <- wkm.(i) -. u
       done;
       if k = m - 1 then raise Exit;
       for i = 0 to k do
         wkm.(i) <- d.(i)
       done;
       for j = 0 to n - k - 3 do
         let v1 = wkm.(k) *. wk2.(j) in
         wk1.(j) <- wk1.(j) -. v1;
         let v2 = wkm.(k) *. wk1.(j + 1) in
         wk2.(j) <- wk2.(j + 1) -. v2
       done
     done;
     raise BadN
   with Exit -> ());
  (d, !xms)

let fixrts d =
  let m = Array.length d in
  if m < 1 then raise BadN;
  let a = Array.make (m + 1) { re = 0.0; im = 0.0 } in
  a.(m) <- { re = 1.0; im = 0.0 };
  for j = 0 to m - 1 do
    a.(j) <- { re = -.d.(m - 1 - j); im = 0.0 }
  done;
  let roots, flag = zroots a in
  if flag <> 0 then raise BadN;
  for j = 0 to m - 1 do
    if cxabs roots.(j) > 1.0 then (
      let c = { re = roots.(j).re; im = -.roots.(j).im } in
      roots.(j) <- cxdiv { re = 1.0; im = 0.0 } c)
  done;
  a.(0) <- { re = -.roots.(0).re; im = -.roots.(0).im };
  a.(1) <- { re = 1.0; im = 0.0 };
  for j = 1 to m - 1 do
    a.(j + 1) <- { re = 1.0; im = 0.0 };
    for i = j downto 1 do
      a.(i) <- cxsub a.(i - 1) (cxmul roots.(j) a.(i))
    done;
    a.(0) <- cxmul { re = -.roots.(j).re; im = -.roots.(j).im } a.(0)
  done;
  let out = Array.make m 0.0 in
  for j = 0 to m - 1 do
    out.(m - 1 - j) <- -.a.(j).re
  done;
  out

let predic data d nfut =
  let ndata = Array.length data and m = Array.length d in
  if ndata < m || m < 1 then raise BadN;
  let reg = Array.init m (fun j -> data.(ndata - 1 - j)) in
  let future = Array.make nfut 0.0 in
  for j = 0 to nfut - 1 do
    let sum = ref 0.0 in
    for k = 0 to m - 1 do
      let t = d.(k) *. reg.(k) in
      sum := !sum +. t
    done;
    for k = m - 1 downto 1 do
      reg.(k) <- reg.(k - 1)
    done;
    reg.(0) <- !sum;
    future.(j) <- !sum
  done;
  future

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
  if op = "MEMCOF" then (
    let n = int_of_string toks.(1) and m = int_of_string toks.(2) in
    let nn = max 0 n in
    let data = Array.init nn (fun i -> float_of_string toks.(3 + i)) in
    (try
       let d, xms = memcof data m in
       let parts = ref [] in
       for i = 0 to m - 1 do
         parts := fmt6 (canon d.(i)) :: !parts
       done;
       parts := fmt6 (canon xms) :: !parts;
       parts := "0" :: !parts;
       print_endline (String.concat " " (List.rev !parts))
     with BadN ->
       let w = max 0 (m + 1) in
       let parts = List.init w (fun _ -> "0.000000") @ ["1"] in
       print_endline (String.concat " " parts)))
  else if op = "FIXRTS" then (
    let m = int_of_string toks.(1) in
    let mm = max 0 m in
    let d = Array.init mm (fun i -> float_of_string toks.(2 + i)) in
    (try
       let out = fixrts d in
       let parts = ref [] in
       for i = 0 to mm - 1 do
         parts := fmt6 (canon out.(i)) :: !parts
       done;
       parts := "0" :: !parts;
       print_endline (String.concat " " (List.rev !parts))
     with BadN ->
       let parts = List.init mm (fun _ -> "0.000000") @ ["1"] in
       print_endline (String.concat " " parts)))
  else if op = "PREDIC" then (
    let ndata = int_of_string toks.(1)
    and m = int_of_string toks.(2)
    and nfut = int_of_string toks.(3) in
    let nn = max 0 ndata and mm = max 0 m and nf = max 0 nfut in
    let data = Array.init nn (fun i -> float_of_string toks.(4 + i)) in
    let d = Array.init mm (fun i -> float_of_string toks.(4 + nn + i)) in
    (try
       let future = predic data d nf in
       let parts = ref [] in
       for i = 0 to nf - 1 do
         parts := fmt6 (canon future.(i)) :: !parts
       done;
       parts := "0" :: !parts;
       print_endline (String.concat " " (List.rev !parts))
     with BadN ->
       let parts = List.init nf (fun _ -> "0.000000") @ ["1"] in
       print_endline (String.concat " " parts)))
