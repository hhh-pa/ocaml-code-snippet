(* Support vector machines: Mangasarian-Musicant 1-norm soft-margin
   variant solved by successive overrelaxation (SOR) with linear,
   polynomial, and Gaussian kernels.
   Reference: Press et al., Numerical Recipes 3e, Section 16.5
   (Svm/Svmgenkernel from svm.h, vendoring Ran from 7.1 and
   Indexx::index from 8.2).
   Documented decisions: nrelax is an explicit input (fixed group
   count, never data-dependent convergence); Ran(21) is reseeded
   per SVMTRAIN exactly as the book's Svm constructor; Indexx is
   the book's quicksort+insertion hybrid verbatim (its tie order
   feeds the update sequence); Svm::relax updates alph[j] with
   (om/(ker+1))*(y*sum-1) and projection MAX(0,MIN(lambda,.));
   y values are used as given (the book assumes +-1 without
   checking); m, n >= 1 and 1 <= nrelax <= 100000 enforced,
   m <= 2048 and n <= 4096 bound the kernel matrix (robustness
   caps; the book checks none of these); unknown kernels flag.
   No online-judge problem found -- fixtures are self-generated
   (KKT/formula oracle), no header link. *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

(* NOTE: float multiplies feeding +. or -. are let-bound first:
   ocamlopt fuses a direct mul-add/sub nest into FMA, which rounds
   once instead of twice and breaks bit-agreement with the C++
   reference (verified: zero FMA in the object code). *)

(* Ran (NR 7.1) needs wrapping 64-bit arithmetic: Int64.mul/add
   wrap, and shifts must be logical (shift_right_logical). *)

type ran = { mutable u : int64; mutable v : int64; mutable w : int64 }

let ran_step r =
  r.u <- Int64.add (Int64.mul r.u 2862933555777941757L) 7046029254386353087L;
  r.v <- Int64.logxor r.v (Int64.shift_right_logical r.v 17);
  r.v <- Int64.logxor r.v (Int64.shift_left r.v 31);
  r.v <- Int64.logxor r.v (Int64.shift_right_logical r.v 8);
  r.w <-
    Int64.add
      (Int64.mul 4294957665L (Int64.logand r.w 0xFFFFFFFFL))
      (Int64.shift_right_logical r.w 32);
  let x1 = Int64.logxor r.u (Int64.shift_left r.u 21) in
  let x2 = Int64.logxor x1 (Int64.shift_right_logical x1 35) in
  let x = Int64.logxor x2 (Int64.shift_left x2 4) in
  Int64.logxor (Int64.add x r.v) r.w

let make_ran j =
  let r = { u = 0L; v = 4101842887655102017L; w = 1L } in
  r.u <- Int64.logxor j r.v;
  ignore (ran_step r);
  r.v <- r.u;
  ignore (ran_step r);
  r.w <- r.v;
  ignore (ran_step r);
  r

let ran_int32u r = Int64.to_int (Int64.logand (ran_step r) 0xFFFFFFFFL)

let indexx arr =
  let m = 7 and nstack = 64 in
  let n = Array.length arr in
  let indx = Array.init n (fun i -> i) in
  let istack = Array.make nstack 0 in
  let ir = ref (n - 1) in
  let l = ref 0 in
  let jstack = ref (-1) in
  let bad = ref false in
  let cont = ref true in
  while !cont && not !bad do
    if !ir - !l < m then (
      let j = ref (!l + 1) in
      while !j <= !ir do
        let indxt = indx.(!j) in
        let a = arr.(indxt) in
        let i = ref (!j - 1) in
        while !i >= !l && arr.(indx.(!i)) > a do
          indx.(!i + 1) <- indx.(!i);
          i := !i - 1
        done;
        indx.(!i + 1) <- indxt;
        j := !j + 1
      done;
      if !jstack < 0 then cont := false
      else (
        ir := istack.(!jstack);
        jstack := !jstack - 1;
        l := istack.(!jstack);
        jstack := !jstack - 1))
    else (
      let k = (!l + !ir) lsr 1 in
      let t = indx.(k) in
      indx.(k) <- indx.(!l + 1);
      indx.(!l + 1) <- t;
      if arr.(indx.(!l)) > arr.(indx.(!ir)) then (
        let t = indx.(!l) in
        indx.(!l) <- indx.(!ir);
        indx.(!ir) <- t);
      if arr.(indx.(!l + 1)) > arr.(indx.(!ir)) then (
        let t = indx.(!l + 1) in
        indx.(!l + 1) <- indx.(!ir);
        indx.(!ir) <- t);
      if arr.(indx.(!l)) > arr.(indx.(!l + 1)) then (
        let t = indx.(!l) in
        indx.(!l) <- indx.(!l + 1);
        indx.(!l + 1) <- t);
      let i = ref (!l + 1) in
      let j = ref !ir in
      let indxt = indx.(!l + 1) in
      let a = arr.(indxt) in
      let part = ref true in
      while !part do
        i := !i + 1;
        while arr.(indx.(!i)) < a do
          i := !i + 1
        done;
        j := !j - 1;
        while arr.(indx.(!j)) > a do
          j := !j - 1
        done;
        if !j < !i then part := false
        else (
          let t = indx.(!i) in
          indx.(!i) <- indx.(!j);
          indx.(!j) <- t)
      done;
      indx.(!l + 1) <- indx.(!j);
      indx.(!j) <- indxt;
      jstack := !jstack + 2;
      if !jstack >= nstack then bad := true
      else if !ir - !i + 1 >= !j - !l then (
        istack.(!jstack) <- !ir;
        istack.(!jstack - 1) <- !i;
        ir := !j - 1)
      else (
        istack.(!jstack) <- !j - 1;
        istack.(!jstack - 1) <- !l;
        l := !i))
  done;
  if !bad then None else Some indx

type kern_kind = KLinear | KPoly | KGauss

type kernel = {
  kind : kern_kind;
  m : int;
  n : int;
  mu : float array;
  pa : float;
  pb : float;
  pd : float;
  ps : float;
  data : float array array;
  ker : float array array;
}

let kern_call k xi xj =
  match k.kind with
  | KLinear ->
    let dott = ref 0.0 in
    for t = 0 to k.n - 1 do
      let p = (xi.(t) -. k.mu.(t)) *. (xj.(t) -. k.mu.(t)) in
      dott := !dott +. p
    done;
    !dott
  | KPoly ->
    let dott = ref 0.0 in
    for t = 0 to k.n - 1 do
      let p = xi.(t) *. xj.(t) in
      dott := !dott +. p
    done;
    let b = k.pa *. !dott in
    (b +. k.pb) ** k.pd
  | KGauss ->
    let dott = ref 0.0 in
    for t = 0 to k.n - 1 do
      let d = xi.(t) -. xj.(t) in
      let p = d *. d in
      dott := !dott +. p
    done;
    exp ((-0.5 *. !dott) /. (k.ps *. k.ps))

let make_kernel kind data pa pb pd ps =
  let m = Array.length data in
  let n = Array.length data.(0) in
  let mu = Array.make n 0.0 in
  for i = 0 to m - 1 do
    for j = 0 to n - 1 do
      mu.(j) <- mu.(j) +. data.(i).(j)
    done
  done;
  for j = 0 to n - 1 do
    mu.(j) <- mu.(j) /. float_of_int m
  done;
  let ker = Array.make_matrix m m 0.0 in
  let k = { kind; m; n; mu; pa; pb; pd; ps; data; ker } in
  for i = 0 to m - 1 do
    for j = 0 to i do
      let v = kern_call k data.(i) data.(j) in
      ker.(i).(j) <- v;
      ker.(j).(i) <- v
    done
  done;
  k

type svm = {
  gker : kernel;
  y : float array;
  m : int;
  alph : float array;
  alphold : float array;
  rng : ran;
  mutable alphinit : bool;
  mutable dalph : float;
}

let make_svm gker y =
  let m = Array.length y in
  {
    gker;
    y;
    m;
    alph = Array.make m 0.0;
    alphold = Array.make m 0.0;
    rng = make_ran 21L;
    alphinit = false;
    dalph = 0.0;
  }

let proj x lambda =
  if x < 0.0 then 0.0 else if x > lambda then lambda else x

let relax s lambda om =
  let m = s.m in
  let y = s.y in
  let ker = s.gker.ker in
  let alph = s.alph in
  if not s.alphinit then (
    for j = 0 to m - 1 do
      alph.(j) <- 0.0
    done;
    s.alphinit <- true);
  Array.blit alph 0 s.alphold 0 m;
  match indexx alph with
  | None -> None
  | Some x -> (
    let fnz = ref 0 in
    while !fnz < m && alph.(x.(!fnz)) = 0.0 do
      fnz := !fnz + 1
    done;
    let j = ref !fnz in
    while !j < m - 2 do
      let k = !j + (ran_int32u s.rng mod (m - !j)) in
      let t = x.(!j) in
      x.(!j) <- x.(k);
      x.(k) <- t;
      j := !j + 1
    done;
    for jj = 0 to m - 1 do
      let j = x.(jj) in
      let sum = ref 0.0 in
      for kk = !fnz to m - 1 do
        let k = x.(kk) in
        let inner = (ker.(j).(k) +. 1.0) *. y.(k) in
        let p = inner *. alph.(k) in
        sum := !sum +. p
      done;
      let ys = y.(j) *. !sum in
      let sub = ys -. 1.0 in
      let step = (om /. (ker.(j).(j) +. 1.0)) *. sub in
      alph.(j) <- proj (alph.(j) -. step) lambda;
      if jj < !fnz && alph.(j) <> 0.0 then (
        fnz := !fnz - 1;
        let t = x.(!fnz) in
        x.(!fnz) <- x.(jj);
        x.(jj) <- t)
    done;
    (match indexx alph with
     | None -> None
     | Some yi -> (
       fnz := 0;
       while !fnz < m && alph.(yi.(!fnz)) = 0.0 do
         fnz := !fnz + 1
       done;
       let fub = ref !fnz in
       while !fub < m && alph.(yi.(!fub)) <> lambda do
         fub := !fub + 1
       done;
       j := !fnz;
       while !j < !fub - 2 do
         let k = !j + (ran_int32u s.rng mod (!fub - !j)) in
         let t = yi.(!j) in
         yi.(!j) <- yi.(k);
         yi.(k) <- t;
         j := !j + 1
       done;
       let pinsum = Array.make m 0.0 in
       for jj = !fnz to !fub - 1 do
         let j = yi.(jj) in
         let sum = ref 0.0 in
         for kk = !fub to m - 1 do
           let k = yi.(kk) in
           let inner = (ker.(j).(k) +. 1.0) *. y.(k) in
           let p = inner *. alph.(k) in
           sum := !sum +. p
         done;
         pinsum.(jj) <- !sum
       done;
       let denom = float_of_int (!fub - !fnz + 1) in
       let niter =
         max
           (int_of_float
              (0.5 *. (float_of_int m +. 1.0)
              *. (float_of_int m -. float_of_int !fnz +. 1.0)
              /. (denom *. denom)))
           1
       in
       for _ = 1 to niter do
         for jj = !fnz to !fub - 1 do
           let j = yi.(jj) in
           let sum = ref pinsum.(jj) in
           for kk = !fnz to !fub - 1 do
             let k = yi.(kk) in
             let inner = (ker.(j).(k) +. 1.0) *. y.(k) in
             let p = inner *. alph.(k) in
             sum := !sum +. p
           done;
           let ys = y.(j) *. !sum in
           let sub = ys -. 1.0 in
           let step = (om /. (ker.(j).(j) +. 1.0)) *. sub in
           alph.(j) <- proj (alph.(j) -. step) lambda
         done
       done;
       let dalph = ref 0.0 in
       for j = 0 to m - 1 do
         let d = alph.(j) -. s.alphold.(j) in
         let p = d *. d in
         dalph := !dalph +. p
       done;
       s.dalph <- !dalph;
       Some (sqrt !dalph))))

let predict_k s kk =
  let sum = ref 0.0 in
  for j = 0 to s.m - 1 do
    let inner = s.alph.(j) *. s.y.(j) in
    let p = inner *. (s.gker.ker.(j).(kk) +. 1.0) in
    sum := !sum +. p
  done;
  !sum

let predict_x s x =
  let sum = ref 0.0 in
  for j = 0 to s.m - 1 do
    let inner = s.alph.(j) *. s.y.(j) in
    let p = inner *. (kern_call s.gker s.gker.data.(j) x +. 1.0) in
    sum := !sum +. p
  done;
  !sum

let parse_kind s =
  if s = "LINEAR" then Some KLinear
  else if s = "POLY" then Some KPoly
  else if s = "GAUSS" then Some KGauss
  else None

let emit a flag =
  let parts = Array.to_list (Array.map (fun v -> fmt6 (canon v)) a) in
  print_endline (String.concat " " (parts @ [ string_of_int flag ]))

let emit_zeros n flag =
  let parts = List.init n (fun _ -> "0.000000") in
  print_endline (String.concat " " (parts @ [ string_of_int flag ]))

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
  if op <> "SVMTRAIN" && op <> "SVMPREDICT" then exit 0;
  let ks = toks.(1) in
  let kind = parse_kind ks in
  let pa = ref 0.0 and pb = ref 0.0 and pd = ref 0.0 and ps = ref 0.0 in
  let pos = ref 2 in
  (match ks with
   | "POLY" ->
     pa := float_of_string toks.(!pos);
     pb := float_of_string toks.(!pos + 1);
     pd := float_of_string toks.(!pos + 2);
     pos := !pos + 3
   | "GAUSS" ->
     ps := float_of_string toks.(!pos);
     pos := !pos + 1
   | _ -> ());
  if op = "SVMTRAIN" then (
    let lambda = float_of_string toks.(!pos) in
    let om = float_of_string toks.(!pos + 1) in
    let nrelax = int_of_string toks.(!pos + 2) in
    let m = int_of_string toks.(!pos + 3) in
    let n = int_of_string toks.(!pos + 4) in
    pos := !pos + 5;
    let shapeok = m >= 1 && m <= 2048 && n >= 1 && n <= 4096 in
    let ok =
      match kind with
      | None -> false
      | Some _ -> shapeok && nrelax >= 1 && nrelax <= 100000
    in
    if ok then (
      let kk =
        match kind with
        | Some x -> x
        | None -> KLinear
      in
      let data = Array.make_matrix m n 0.0 in
      for i = 0 to m - 1 do
        for j = 0 to n - 1 do
          data.(i).(j) <- float_of_string toks.(!pos);
          pos := !pos + 1
        done
      done;
      let y = Array.init m (fun i ->
        let v = float_of_string toks.(!pos + i) in
        v) in
      pos := !pos + m;
      let kern = make_kernel kk data !pa !pb !pd !ps in
      let s = make_svm kern y in
      let dalph = ref 0.0 in
      let good = ref true in
      let r = ref 0 in
      while !r < nrelax && !good do
        (match relax s lambda om with
         | None -> good := false
         | Some d -> dalph := d);
        r := !r + 1
      done;
      if not !good then emit_zeros (2 * m + 1) 1
      else (
        let out = Array.make (2 * m + 1) 0.0 in
        for i = 0 to m - 1 do
          out.(i) <- s.alph.(i)
        done;
        out.(m) <- !dalph;
        for i = 0 to m - 1 do
          out.(m + 1 + i) <- predict_k s i
        done;
        emit out 0))
    else emit_zeros (if shapeok then 2 * m + 1 else 0) 1)
  else (
    let m = int_of_string toks.(!pos) in
    let n = int_of_string toks.(!pos + 1) in
    pos := !pos + 2;
    let shapeok = m >= 1 && m <= 2048 && n >= 1 && n <= 4096 in
    let ok =
      match kind with
      | None -> false
      | Some _ -> shapeok
    in
    if ok then (
      let kk =
        match kind with
        | Some x -> x
        | None -> KLinear
      in
      let data = Array.make_matrix m n 0.0 in
      for i = 0 to m - 1 do
        for j = 0 to n - 1 do
          data.(i).(j) <- float_of_string toks.(!pos);
          pos := !pos + 1
        done
      done;
      let y = Array.init m (fun i -> float_of_string toks.(!pos + i)) in
      pos := !pos + m;
      let alph = Array.init m (fun i -> float_of_string toks.(!pos + i)) in
      pos := !pos + m;
      let x = Array.init n (fun i -> float_of_string toks.(!pos + i)) in
      let kern = make_kernel kk data !pa !pb !pd !ps in
      let s = make_svm kern y in
      Array.blit alph 0 s.alph 0 m;
      s.alphinit <- true;
      emit [| predict_x s x |] 0)
    else emit_zeros (if shapeok then 1 else 0) 1)
