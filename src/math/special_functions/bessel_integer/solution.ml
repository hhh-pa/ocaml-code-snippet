(* Integer Bessel functions (NR 6.5). *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v
let euler = 0.577215664901533
let pi = 3.141592653589793

let j0s x =
  let q = x *. x /. 4.0 in
  let term = ref 1.0 and sum = ref 1.0 in
  for k = 1 to 40 do
    term := !term *. -.q /. (float k *. float k);
    sum := !sum +. !term
  done;
  !sum

let j1s x =
  let q = x *. x /. 4.0 in
  let term = ref 1.0 and sum = ref 1.0 in
  for k = 1 to 40 do
    term := !term *. -.q /. (float k *. (float k +. 1.0));
    sum := !sum +. !term
  done;
  0.5 *. x *. !sum

let pqjy x nu =
  let mu = 4.0 *. float nu *. float nu in
  let r = 1.0 /. (8.0 *. x) in
  let c = ref 1.0 and pw = ref 1.0 and p = ref 1.0 and q = ref 0.0 in
  for k = 1 to 10 do
    c := !c *. (mu -. (2.0 *. float k -. 1.0) *. (2.0 *. float k -. 1.0)) /. float k;
    pw := !pw *. r;
    if k mod 2 = 0 then p := !p +. (if (k / 2) mod 2 = 0 then 1.0 else -1.0) *. !c *. !pw
    else q := !q +. (if ((k - 1) / 2) mod 2 = 0 then 1.0 else -1.0) *. !c *. !pw
  done;
  (!p, !q)

let j0f x =
  let ax = abs_float x in
  if ax < 8.0 then j0s x
  else (
    let p, q = pqjy ax 0 in
    let chi = ax -. pi /. 4.0 in
    sqrt (2.0 /. (pi *. ax)) *. (p *. cos chi -. q *. sin chi))

let j1f x =
  let ax = abs_float x in
  let v =
    if ax < 8.0 then j1s ax
    else (
      let p, q = pqjy ax 1 in
      let chi = ax -. pi /. 2.0 -. pi /. 4.0 in
      sqrt (2.0 /. (pi *. ax)) *. (p *. cos chi -. q *. sin chi))
  in
  if x < 0.0 then -.v else v

let y0s x =
  let q = x *. x /. 4.0 in
  let m = ref 1.0 and h = ref 0.0 and s = ref 0.0 and sgn = ref 1.0 in
  for k = 1 to 40 do
    m := !m *. q /. (float k *. float k);
    h := !h +. 1.0 /. float k;
    s := !s +. !sgn *. !h *. !m;
    sgn := -. !sgn
  done;
  (2.0 /. pi) *. ((log (x /. 2.0) +. euler) *. j0s x +. !s)

let y1s x =
  let pw = ref (x /. 2.0) and h = ref 0.0 and s = ref 0.0 and sgn = ref 1.0 in
  for k = 0 to 40 do
    let hp1 = !h +. 1.0 /. (float k +. 1.0) in
    s := !s +. !sgn *. (!h +. hp1) *. !pw;
    h := hp1;
    sgn := -. !sgn;
    pw := !pw *. (x *. x /. 4.0) /. ((float k +. 1.0) *. (float k +. 2.0))
  done;
  (2.0 /. pi) *. (log (x /. 2.0) +. euler) *. j1s x -. 2.0 /. (pi *. x) -. !s /. pi

let y0f x =
  if x < 8.0 then y0s x
  else (
    let p, q = pqjy x 0 in
    let chi = x -. pi /. 4.0 in
    sqrt (2.0 /. (pi *. x)) *. (p *. sin chi +. q *. cos chi))

let y1f x =
  if x < 8.0 then y1s x
  else (
    let p, q = pqjy x 1 in
    let chi = x -. pi /. 2.0 -. pi /. 4.0 in
    sqrt (2.0 /. (pi *. x)) *. (p *. sin chi +. q *. cos chi))

let i0s x =
  let q = x *. x /. 4.0 in
  let term = ref 1.0 and sum = ref 1.0 in
  for k = 1 to 150 do
    term := !term *. q /. (float k *. float k);
    sum := !sum +. !term
  done;
  !sum

let i1s x =
  let q = x *. x /. 4.0 in
  let term = ref 1.0 and sum = ref 1.0 in
  for k = 1 to 150 do
    term := !term *. q /. (float k *. (float k +. 1.0));
    sum := !sum +. !term
  done;
  0.5 *. x *. !sum

let aser x nu nterms alt =
  let mu = 4.0 *. float nu *. float nu in
  let r = 1.0 /. (8.0 *. x) in
  let c = ref 1.0 and pw = ref 1.0 and s = ref 1.0 in
  for k = 1 to nterms do
    c := !c *. (mu -. (2.0 *. float k -. 1.0) *. (2.0 *. float k -. 1.0)) /. float k;
    pw := !pw *. r;
    s := !s +. (if alt && k mod 2 = 1 then -1.0 else 1.0) *. !c *. !pw
  done;
  !s

let i0f x =
  let ax = abs_float x in
  if ax < 15.0 then i0s x else exp ax /. sqrt (2.0 *. pi *. ax) *. aser ax 0 20 true

let i1f x =
  let ax = abs_float x in
  let v =
    if ax < 15.0 then i1s ax
    else exp ax /. sqrt (2.0 *. pi *. ax) *. aser ax 1 20 true
  in
  if x < 0.0 then -.v else v

let k0s x =
  let q = x *. x /. 4.0 in
  let m = ref 1.0 and h = ref 0.0 and s = ref 0.0 in
  for k = 1 to 60 do
    m := !m *. q /. (float k *. float k);
    h := !h +. 1.0 /. float k;
    s := !s +. !h *. !m
  done;
  -.(log (x /. 2.0) +. euler) *. i0s x +. !s

let k1s x =
  let pw = ref (x /. 2.0) and h = ref 0.0 and s = ref 0.0 in
  for k = 0 to 60 do
    let hp1 = !h +. 1.0 /. (float k +. 1.0) in
    s := !s +. (!h +. hp1) *. !pw;
    h := hp1;
    pw := !pw *. (x *. x /. 4.0) /. ((float k +. 1.0) *. (float k +. 2.0))
  done;
  1.0 /. x +. (log (x /. 2.0) +. euler) *. i1s x -. 0.5 *. !s

let k0f x =
  if x < 6.0 then k0s x
  else sqrt (pi /. (2.0 *. x)) *. exp (-.x) *. aser x 0 16 false

let k1f x =
  if x < 6.0 then k1s x
  else sqrt (pi /. (2.0 *. x)) *. exp (-.x) *. aser x 1 16 false

let frexp_k x =
  if x = 0.0 then -1000
  else (
    let _, e = frexp x in
    e)

let ldexp_m512 x = ldexp x (-512)

let bjn n x =
  if n = 0 then j0f x
  else if n = 1 then j1f x
  else (
    let ax = abs_float x in
    if ax *. ax <= 8.0 *. 2.2250738585072014e-308 then 0.0
    else (
      let ans =
        if ax > float n then (
          let tox = 2.0 /. ax in
          let bjm = ref (j0f ax) and bj = ref (j1f ax) in
          for j = 1 to n - 1 do
            let bjp = float j *. tox *. !bj -. !bjm in
            bjm := !bj;
            bj := bjp
          done;
          !bj)
        else (
          let tox = 2.0 /. ax in
          let m = 2 * ((n + int_of_float (sqrt (160.0 *. float n))) / 2) in
          let jsum = ref false and bjp = ref 0.0 and bj = ref 1.0 and sum = ref 0.0 and ans = ref 0.0 in
          for j = m downto 1 do
            let bjm = float j *. tox *. !bj -. !bjp in
            bjp := !bj;
            bj := bjm;
            if frexp_k !bj > 512 then (
              bj := ldexp_m512 !bj;
              bjp := ldexp_m512 !bjp;
              ans := ldexp_m512 !ans;
              sum := ldexp_m512 !sum);
            if !jsum then sum := !sum +. !bj;
            jsum := not !jsum;
            if j = n then ans := !bjp
          done;
          sum := 2.0 *. !sum -. !bj;
          !ans /. !sum)
      in
      if x < 0.0 && n mod 2 <> 0 then -.ans else ans))

let byn n x =
  if n = 0 then y0f x
  else if n = 1 then y1f x
  else (
    let tox = 2.0 /. x in
    let by = ref (y1f x) and bym = ref (y0f x) in
    for j = 1 to n - 1 do
      let byp = float j *. tox *. !by -. !bym in
      bym := !by;
      by := byp
    done;
    !by)

let binf n x =
  if n = 0 then i0f x
  else if n = 1 then i1f x
  else if x *. x <= 8.0 *. 2.2250738585072014e-308 then 0.0
  else (
    let tox = 2.0 /. abs_float x in
    let bip = ref 0.0 and bi = ref 1.0 and ans = ref 0.0 in
    for j = 2 * (n + int_of_float (sqrt (200.0 *. float n))) downto 1 do
      let bim = !bip +. float j *. tox *. !bi in
      bip := !bi;
      bi := bim;
      if frexp_k !bi > 512 then (
        ans := ldexp_m512 !ans;
        bi := ldexp_m512 !bi;
        bip := ldexp_m512 !bip);
      if j = n then ans := !bip
    done;
    ans := !ans *. i0f x /. !bi;
    if x < 0.0 && n mod 2 <> 0 then -. !ans else !ans)

let bkn n x =
  if n = 0 then k0f x
  else if n = 1 then k1f x
  else (
    let tox = 2.0 /. x in
    let bkm = ref (k0f x) and bk = ref (k1f x) in
    for j = 1 to n - 1 do
      let bkp = !bkm +. float j *. tox *. !bk in
      bkm := !bk;
      bk := bkp
    done;
    !bk)

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
  let next_tok () =
    let v = toks.(!pos) in
    incr pos; v
  in
  let op = next_tok () in
  let ns = next_tok () in
  let xs = next_tok () in
  let n = int_of_string ns and x = float_of_string xs in
  let ans = ref 0.0 in
  (if op = "JN" then ans := bjn n x
   else if op = "YN" then ans := byn n x
   else if op = "IN" then ans := binf n x
   else ans := bkn n x);
  print_endline (fmt6 (canon !ans))
