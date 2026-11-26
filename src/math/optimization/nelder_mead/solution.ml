(* Multidimensional minimization by the downhill simplex
   method of Nelder and Mead (Amoeba).
   Reference: Press et al., Numerical Recipes 3e, Section 10.5.
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

exception TooMany of int

let minimize point del ftol =
  let nmax = 5000 in
  let tiny = 1.0e-10 in
  let n = Array.length point in
  let mpts = n + 1 and ndim = n in
  let p = Array.init mpts (fun i ->
    let r = Array.copy point in
    (if i <> 0 then r.(i - 1) <- r.(i - 1) +. del);
    r) in
  let y = Array.make mpts 0.0 in
  for i = 0 to mpts - 1 do
    y.(i) <- func p.(i)
  done;
  let nfunc = ref 0 in
  let psum = Array.make ndim 0.0 in
  let get_psum () =
    for j = 0 to ndim - 1 do
      let s = ref 0.0 in
      for i = 0 to mpts - 1 do
        s := !s +. p.(i).(j)
      done;
      psum.(j) <- !s
    done
  in
  let amotry ihi fac =
    let fac1 = (1.0 -. fac) /. float ndim in
    let fac2 = fac1 -. fac in
    let ptry = Array.make ndim 0.0 in
    for j = 0 to ndim - 1 do
      ptry.(j) <- psum.(j) *. fac1 -. p.(ihi).(j) *. fac2
    done;
    let ytry = func ptry in
    (if ytry < y.(ihi) then (
       y.(ihi) <- ytry;
       for j = 0 to ndim - 1 do
         psum.(j) <- psum.(j) +. ptry.(j) -. p.(ihi).(j);
         p.(ihi).(j) <- ptry.(j)
       done));
    ytry
  in
  get_psum ();
  let rec loop () =
    let ilo = ref 0 in
    let ihi = ref 0 and inhi = ref 0 in
    (if y.(0) > y.(1) then (inhi := 1; ihi := 0) else (inhi := 0; ihi := 1));
    for i = 0 to mpts - 1 do
      (if y.(i) <= y.(!ilo) then ilo := i);
      if y.(i) > y.(!ihi) then (inhi := !ihi; ihi := i)
      else if y.(i) > y.(!inhi) && i <> !ihi then inhi := i
    done;
    let rtol =
      2.0 *. abs_float (y.(!ihi) -. y.(!ilo))
      /. (abs_float y.(!ihi) +. abs_float y.(!ilo) +. tiny)
    in
    if rtol < ftol then (
      let tmp = y.(0) in
      y.(0) <- y.(!ilo);
      y.(!ilo) <- tmp;
      let pmin = Array.make ndim 0.0 in
      for i = 0 to ndim - 1 do
        let t2 = p.(0).(i) in
        p.(0).(i) <- p.(!ilo).(i);
        p.(!ilo).(i) <- t2;
        pmin.(i) <- p.(0).(i)
      done;
      (pmin, y.(0), !nfunc))
    else (
      (if !nfunc >= nmax then raise (TooMany !nfunc));
      nfunc := !nfunc + 2;
      let ytry = amotry !ihi (-1.0) in
      (if ytry <= y.(!ilo) then (
         let _ = amotry !ihi 2.0 in
         ())
       else if ytry >= y.(!inhi) then (
         let ysave = y.(!ihi) in
         let ytry2 = amotry !ihi 0.5 in
         if ytry2 >= ysave then (
           for i = 0 to mpts - 1 do
             if i <> !ilo then (
               for j = 0 to ndim - 1 do
                 let v = 0.5 *. (p.(i).(j) +. p.(!ilo).(j)) in
                 p.(i).(j) <- v;
                 psum.(j) <- v
               done;
               y.(i) <- func psum)
           done;
           nfunc := !nfunc + ndim;
           get_psum ()))
       else nfunc := !nfunc - 1);
      loop ())
  in
  loop ()

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
  let del = float_of_string toks.(!p + n) and ftol = float_of_string toks.(!p + n + 1) in
  if op = "AMOEBA" then
    try
      let pmin, fmin, nfunc = minimize point del ftol in
      let parts = Array.to_list (Array.init n (fun i -> fmt6 (canon pmin.(i)))) in
      print_endline
        (String.concat " " parts ^ " " ^ fmt6 (canon fmin) ^ " " ^ string_of_int nfunc ^ " 0")
    with TooMany nf ->
      let parts = Array.to_list (Array.init n (fun _ -> "0.000000")) in
      print_endline (String.concat " " parts ^ " 0.000000 " ^ string_of_int nf ^ " 1")
