(* Fractional Bessel, Airy, spherical Bessel (NR 6.6). *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v
let pi = 3.141592653589793

let gcof =
  [| 57.1562356658629235; -59.5979603554754912; 14.1360979747417471;
     -0.491913816097620199; 0.339946499848118887e-4; 0.465236289270485756e-4;
     -0.983744753048795646e-4; 0.158088703224912494e-3; -0.210264441724104883e-3;
     0.217439618115212643e-3; -0.164318106536763890e-3; 0.844182239838527433e-4;
     -0.261908384015814087e-4; 0.368991826595316234e-5 |]

let gammln xx =
  let y = ref xx in
  let x = xx in
  let tmp = ref (x +. 5.2421875000000000) in
  tmp := (x +. 0.5) *. log !tmp -. !tmp;
  let ser = ref 0.999999999999997092 in
  for j = 0 to 13 do
    y := !y +. 1.0;
    ser := !ser +. gcof.(j) /. !y
  done;
  !tmp +. log (2.5066282746310005 *. !ser /. x)

let gammafn x =
  let prod = ref 1.0 and t = ref x in
  while !t <= 0.0 do
    prod := !prod *. !t;
    t := !t +. 1.0
  done;
  exp (gammln !t) /. !prod

let jfrac nu x =
  let h = x /. 2.0 in
  let t = ref (h ** nu /. gammafn (nu +. 1.0)) in
  let s = ref !t in
  let q = h *. h in
  for k = 1 to 80 do
    t := !t *. -.q /. (float k *. (float k +. nu));
    s := !s +. !t
  done;
  !s

let yfrac nu x =
  (jfrac nu x *. cos (nu *. pi) -. jfrac (-.nu) x) /. sin (nu *. pi)

let ifrac nu x =
  let h = x /. 2.0 in
  let t = ref (h ** nu /. gammafn (nu +. 1.0)) in
  let s = ref !t in
  let q = h *. h in
  for k = 1 to 80 do
    t := !t *. q /. (float k *. (float k +. nu));
    s := !s +. !t
  done;
  !s

let aserf x nu nterms alt =
  let mu = 4.0 *. nu *. nu in
  let r = 1.0 /. (8.0 *. x) in
  let c = ref 1.0 and pw = ref 1.0 and s = ref 1.0 in
  for k = 1 to nterms do
    c := !c *. (mu -. (2.0 *. float k -. 1.0) *. (2.0 *. float k -. 1.0)) /. float k;
    pw := !pw *. r;
    s := !s +. (if alt && k mod 2 = 1 then -1.0 else 1.0) *. !c *. !pw
  done;
  !s

let kfrac nu x =
  if x < 6.0 then pi *. (ifrac (-.nu) x -. ifrac nu x) /. (2.0 *. sin (nu *. pi))
  else sqrt (pi /. (2.0 *. x)) *. exp (-.x) *. aserf x nu 16 false

let airy x =
  let onovrt = 0.577350269189626 in
  if x = 0.0 then (
    let g23 = gammafn (2.0 /. 3.0) and g13 = gammafn (1.0 /. 3.0) in
    ( 1.0 /. (3.0 ** (2.0 /. 3.0) *. g23),
      1.0 /. (3.0 ** (1.0 /. 6.0) *. g23),
      -1.0 /. (3.0 ** (1.0 /. 3.0) *. g13),
      3.0 ** (1.0 /. 6.0) /. g13 ))
  else (
    let ax = abs_float x in
    let rootx = sqrt ax in
    let z = (2.0 /. 3.0) *. ax *. rootx in
    if x > 0.0 then (
      let io = ifrac (1.0 /. 3.0) z and ko = kfrac (1.0 /. 3.0) z in
      let ai = rootx *. onovrt *. ko /. pi in
      let bi = rootx *. (ko /. pi +. 2.0 *. onovrt *. io) in
      let io2 = ifrac (2.0 /. 3.0) z and ko2 = kfrac (2.0 /. 3.0) z in
      let aip = -.x *. onovrt *. ko2 /. pi in
      let bip = x *. (ko2 /. pi +. 2.0 *. onovrt *. io2) in
      (ai, bi, aip, bip))
    else (
      let jo = jfrac (1.0 /. 3.0) z and yo = yfrac (1.0 /. 3.0) z in
      let ai = 0.5 *. rootx *. (jo -. onovrt *. yo) in
      let bi = -0.5 *. rootx *. (yo +. onovrt *. jo) in
      let jo2 = jfrac (2.0 /. 3.0) z and yo2 = yfrac (2.0 /. 3.0) z in
      let aip = 0.5 *. ax *. (jo2 +. onovrt *. yo2) in
      let bip = 0.5 *. ax *. (onovrt *. jo2 -. yo2) in
      (ai, bi, aip, bip)))

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
  let ans = ref 0.0 in
  (if op = "JFRAC" || op = "YFRAC" || op = "IFRAC" || op = "KFRAC" then (
     let nu = float_of_string (next_tok ()) in
     let x = float_of_string (next_tok ()) in
     ans :=
       (if op = "JFRAC" then jfrac nu x
        else if op = "YFRAC" then yfrac nu x
        else if op = "IFRAC" then ifrac nu x
        else kfrac nu x))
   else if op = "AI" || op = "BI" || op = "AIP" || op = "BIP" then (
     let x = float_of_string (next_tok ()) in
     let ai, bi, aip, bip = airy x in
     ans :=
       (if op = "AI" then ai else if op = "BI" then bi else if op = "AIP" then aip else bip))
   else (
     let n = int_of_string (next_tok ()) in
     let x = float_of_string (next_tok ()) in
     let rtpio2 = 1.253314137315500251 in
     let nu = float n +. 0.5 in
     let factor = rtpio2 /. sqrt x in
     let jo = jfrac nu x and yo = yfrac nu x in
     let jo1 = jfrac (nu +. 1.0) x and yo1 = yfrac (nu +. 1.0) x in
     let jpo = (nu /. x) *. jo -. jo1 and ypo = (nu /. x) *. yo -. yo1 in
     let sj = factor *. jo and sy = factor *. yo in
     ans :=
       (if op = "SPHJ" then sj
        else if op = "SPHY" then sy
        else if op = "SPHJP" then factor *. jpo -. sj /. (2.0 *. x)
        else factor *. ypo -. sy /. (2.0 *. x))));
  print_endline (fmt6 (canon !ans))
