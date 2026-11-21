(* Gaussian quadrature rules (NR 4.6). Port of the C++ reference. *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v
let fmtxw x w = fmt6 (canon x) ^ " " ^ fmt6 (canon w)

let gamm_c =
  [| 76.18009172947146; -86.50532032941677; 24.01409824083091; -1.231739572450155;
     0.1208650973866179e-2; -0.5395239384953e-5 |]

let gammln xx =
  let y = ref xx in
  let tmp = ref (xx +. 5.5) in
  let ser = ref 1.000000000190015 in
  tmp := !tmp -. (xx +. 0.5) *. log !tmp;
  for j = 0 to 5 do
    y := !y +. 1.0;
    ser := !ser +. gamm_c.(j) /. !y
  done;
  -. !tmp +. log (2.5066282746310005 *. !ser /. xx)

let eps = 1.0e-14

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
  if op = "LEGENDRE" then (
    let x1 = float_of_string (next_tok ()) in
    let x2 = float_of_string (next_tok ()) in
    let n = int_of_string (next_tok ()) in
    let x = Array.make n 0.0 and w = Array.make n 0.0 in
    let m = (n + 1) / 2 in
    let xm = 0.5 *. (x2 +. x1) and xl = 0.5 *. (x2 -. x1) in
    for i = 0 to m - 1 do
      let z = ref (cos (3.141592654 *. (float i +. 0.75) /. (float n +. 0.5))) in
      let pp = ref 0.0 in
      (try
         while true do
           let p1 = ref 1.0 and p2 = ref 0.0 in
           for j = 0 to n - 1 do
             let p3 = !p2 in
             p2 := !p1;
             p1 := ((2.0 *. float j +. 1.0) *. !z *. !p2 -. float j *. p3) /. float (j + 1)
           done;
           pp := float n *. (!z *. !p1 -. !p2) /. (!z *. !z -. 1.0);
           let z1 = !z in
           z := z1 -. !p1 /. !pp;
           if abs_float (!z -. z1) <= eps then raise Exit
         done
       with Exit -> ());
      x.(i) <- xm -. xl *. !z;
      x.(n - 1 - i) <- xm +. xl *. !z;
      w.(i) <- 2.0 *. xl /. ((1.0 -. !z *. !z) *. !pp *. !pp);
      w.(n - 1 - i) <- w.(i)
    done;
    for i = 0 to n - 1 do
      print_endline (fmtxw x.(i) w.(i))
    done)
  else if op = "LAGUERRE" then (
    let alf = float_of_string (next_tok ()) in
    let n = int_of_string (next_tok ()) in
    let maxit = 10 in
    let x = Array.make n 0.0 and w = Array.make n 0.0 in
    let z = ref 0.0 in
    for i = 0 to n - 1 do
      (if i = 0 then
         z := (1.0 +. alf) *. (3.0 +. 0.92 *. alf) /. (1.0 +. 2.4 *. float n +. 1.8 *. alf)
       else if i = 1 then
         z := !z +. (15.0 +. 6.25 *. alf) /. (1.0 +. 0.9 *. alf +. 2.5 *. float n)
       else (
         let ai = float (i - 1) in
         z :=
           !z
           +. ((1.0 +. 2.55 *. ai) /. (1.9 *. ai) +. 1.26 *. ai *. alf /. (1.0 +. 3.5 *. ai))
              *. (!z -. x.(i - 2))
              /. (1.0 +. 0.3 *. alf)));
      let pp = ref 0.0 and p2r = ref 0.0 in
      (try
         for _its = 0 to maxit - 1 do
           let p1 = ref 1.0 in
           p2r := 0.0;
           for j = 0 to n - 1 do
             let p3 = !p2r in
             p2r := !p1;
             p1 :=
               ((2.0 *. float j +. 1.0 +. alf -. !z) *. !p2r -. (float j +. alf) *. p3)
               /. float (j + 1)
           done;
           pp := (float n *. !p1 -. (float n +. alf) *. !p2r) /. !z;
           let z1 = !z in
           z := z1 -. !p1 /. !pp;
           if abs_float (!z -. z1) <= eps then raise Exit
         done
       with Exit -> ());
      x.(i) <- !z;
      w.(i) <- -.exp (gammln (alf +. float n) -. gammln (float n)) /. (!pp *. float n *. !p2r)
    done;
    for i = 0 to n - 1 do
      print_endline (fmtxw x.(i) w.(i))
    done)
  else if op = "HERMITE" then (
    let n = int_of_string (next_tok ()) in
    let pim4 = 0.7511255444649425 in
    let maxit = 10 in
    let x = Array.make n 0.0 and w = Array.make n 0.0 in
    let m = (n + 1) / 2 in
    let z = ref 0.0 in
    for i = 0 to m - 1 do
      (if i = 0 then
         z := sqrt (float (2 * n + 1)) -. 1.85575 *. (float (2 * n + 1) ** -0.16667)
       else if i = 1 then z := !z -. 1.14 *. (float n ** 0.426) /. !z
       else if i = 2 then z := 1.86 *. !z -. 0.86 *. x.(0)
       else if i = 3 then z := 1.91 *. !z -. 0.91 *. x.(1)
       else z := 2.0 *. !z -. x.(i - 2));
      let pp = ref 0.0 in
      (try
         for _its = 0 to maxit - 1 do
           let p1 = ref pim4 and p2 = ref 0.0 in
           for j = 0 to n - 1 do
             let p3 = !p2 in
             p2 := !p1;
             p1 :=
               !z *. sqrt (2.0 /. float (j + 1)) *. !p2
               -. sqrt (float j /. float (j + 1)) *. p3
           done;
           pp := sqrt (float (2 * n)) *. !p2;
           let z1 = !z in
           z := z1 -. !p1 /. !pp;
           if abs_float (!z -. z1) <= eps then raise Exit
         done
       with Exit -> ());
      x.(i) <- !z;
      x.(n - 1 - i) <- -. !z;
      w.(i) <- 2.0 /. (!pp *. !pp);
      w.(n - 1 - i) <- w.(i)
    done;
    for i = 0 to n - 1 do
      print_endline (fmtxw x.(i) w.(i))
    done)
  else (
    let alf = float_of_string (next_tok ()) in
    let bet = float_of_string (next_tok ()) in
    let n = int_of_string (next_tok ()) in
    let maxit = 10 in
    let x = Array.make n 0.0 and w = Array.make n 0.0 in
    let z = ref 0.0 in
    for i = 0 to n - 1 do
      (if i = 0 then (
         let an = alf /. float n and bn = bet /. float n in
         let r1 =
           (1.0 +. alf) *. (2.78 /. (4.0 +. float (n * n)) +. 0.768 *. an /. float n)
         in
         let r2 =
           1.0 +. 1.48 *. an +. 0.96 *. bn +. 0.452 *. an *. an +. 0.83 *. an *. bn
         in
         z := 1.0 -. r1 /. r2)
       else if i = 1 then (
         let r1 = (4.1 +. alf) /. ((1.0 +. alf) *. (1.0 +. 0.156 *. alf)) in
         let r2 = 1.0 +. 0.06 *. (float n -. 8.0) *. (1.0 +. 0.12 *. alf) /. float n in
         let r3 = 1.0 +. 0.012 *. bet *. (1.0 +. 0.25 *. abs_float alf) /. float n in
         z := !z -. (1.0 -. !z) *. r1 *. r2 *. r3)
       else if i = 2 then (
         let r1 = (1.67 +. 0.28 *. alf) /. (1.0 +. 0.37 *. alf) in
         let r2 = 1.0 +. 0.22 *. (float n -. 8.0) /. float n in
         let r3 = 1.0 +. 8.0 *. bet /. ((6.28 +. bet) *. float (n * n)) in
         z := !z -. (x.(0) -. !z) *. r1 *. r2 *. r3)
       else if i = n - 2 then (
         let r1 = (1.0 +. 0.235 *. bet) /. (0.766 +. 0.119 *. bet) in
         let r2 = 1.0 /. (1.0 +. 0.639 *. (float n -. 4.0) /. (1.0 +. 0.71 *. (float n -. 4.0))) in
         let r3 = 1.0 /. (1.0 +. 20.0 *. alf /. ((7.5 +. alf) *. float (n * n))) in
         z := !z +. (!z -. x.(n - 4)) *. r1 *. r2 *. r3)
       else if i = n - 1 then (
         let r1 = (1.0 +. 0.37 *. bet) /. (1.67 +. 0.28 *. bet) in
         let r2 = 1.0 /. (1.0 +. 0.22 *. (float n -. 8.0) /. float n) in
         let r3 = 1.0 /. (1.0 +. 8.0 *. alf /. ((6.28 +. alf) *. float (n * n))) in
         z := !z +. (!z -. x.(n - 3)) *. r1 *. r2 *. r3)
       else z := 3.0 *. x.(i - 1) -. 3.0 *. x.(i - 2) +. x.(i - 3));
      let alfbet = alf +. bet in
      let temp = ref (2.0 +. alfbet) in
      let pp = ref 0.0 and p2r = ref 1.0 in
      (try
         for _its = 1 to maxit do
           temp := 2.0 +. alfbet;
           let p1 = ref ((alf -. bet +. !temp *. !z) /. 2.0) in
           p2r := 1.0;
           for j = 2 to n do
             let p3 = !p2r in
             p2r := !p1;
             temp := 2.0 *. float j +. alfbet;
             let a = 2.0 *. float j *. (float j +. alfbet) *. (!temp -. 2.0) in
             let b =
               (!temp -. 1.0)
               *. (alf *. alf -. bet *. bet +. !temp *. (!temp -. 2.0) *. !z)
             in
             let c =
               2.0 *. (float j -. 1.0 +. alf) *. (float j -. 1.0 +. bet) *. !temp
             in
             p1 := (b *. !p2r -. c *. p3) /. a
           done;
           pp :=
             (float n *. (alf -. bet -. !temp *. !z) *. !p1
             +. 2.0 *. (float n +. alf) *. (float n +. bet) *. !p2r)
             /. (!temp *. (1.0 -. !z *. !z));
           let z1 = !z in
           z := z1 -. !p1 /. !pp;
           if abs_float (!z -. z1) <= eps then raise Exit
         done
       with Exit -> ());
      x.(i) <- !z;
      w.(i) <-
        exp
          (gammln (alf +. float n)
          +. gammln (bet +. float n)
          -. gammln (float n +. 1.0)
          -. gammln (float n +. alfbet +. 1.0))
        *. !temp *. (2.0 ** alfbet) /. (!pp *. !p2r)
    done;
    for i = 0 to n - 1 do
      print_endline (fmtxw x.(i) w.(i))
    done)
