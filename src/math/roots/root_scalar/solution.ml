(* Scalar root finding (NR 9.1-9.4). *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

let fn = ref ""
let fp = ref [||]

let func t =
  if !fn = "POLY" then (
    let s = ref 0.0 in
    for i = Array.length !fp - 1 downto 0 do
      s := !s *. t +. !fp.(i)
    done;
    !s)
  else if !fn = "EXP" then !fp.(0) *. exp (!fp.(1) *. t) +. !fp.(2)
  else if !fn = "TRIG" then !fp.(0) *. sin (!fp.(1) *. t +. !fp.(2))
  else if !fn = "LOG" then !fp.(0) *. log (!fp.(1) *. t +. !fp.(2))
  else !fp.(0) *. ((!fp.(1) *. t +. !fp.(2)) ** !fp.(3))

let funcd t =
  if !fn = "POLY" then (
    let s = ref 0.0 in
    for i = Array.length !fp - 1 downto 1 do
      s := !s *. t +. float i *. !fp.(i)
    done;
    !s)
  else if !fn = "EXP" then !fp.(0) *. !fp.(1) *. exp (!fp.(1) *. t)
  else if !fn = "TRIG" then !fp.(0) *. !fp.(1) *. cos (!fp.(1) *. t +. !fp.(2))
  else if !fn = "LOG" then !fp.(0) *. !fp.(1) /. (!fp.(1) *. t +. !fp.(2))
  else !fp.(0) *. !fp.(3) *. !fp.(1) *. ((!fp.(1) *. t +. !fp.(2)) ** (!fp.(3) -. 1.0))

let rtbis x1 x2 xacc =
  let f = func x1 and fmid0 = func x2 in
  if f *. fmid0 >= 0.0 then (0.0, 1)
  else (
    let dx = ref 0.0 and rtb = ref 0.0 in
    (if f < 0.0 then (
       dx := x2 -. x1;
       rtb := x1)
     else (
       dx := x1 -. x2;
       rtb := x2));
    let res = ref None and j = ref 0 in
    while !j < 50 && !res = None do
      dx := !dx *. 0.5;
      let xmid = !rtb +. !dx in
      let fmid = func xmid in
      (if fmid <= 0.0 then rtb := xmid);
      (if abs_float !dx < xacc || fmid = 0.0 then res := Some (!rtb, 0));
      j := !j + 1
    done;
    (match !res with Some v -> v | None -> (!rtb, 1)))

let rtflsp x1 x2 xacc =
  let fl = ref (func x1) and fh = ref (func x2) in
  if !fl *. !fh > 0.0 then (0.0, 1)
  else (
    let xl = ref 0.0 and xh = ref 0.0 in
    (if !fl < 0.0 then (
       xl := x1;
       xh := x2)
     else (
       xl := x2;
       xh := x1;
       let t = !fl in
       fl := !fh;
       fh := t));
    let dx = ref (!xh -. !xl) and rtf = ref !xl in
    let res = ref None and j = ref 0 in
    while !j < 30 && !res = None do
      rtf := !xl +. !dx *. !fl /. (!fl -. !fh);
      let fv = func !rtf in
      let del =
        if fv < 0.0 then (
          let d = !xl -. !rtf in
          xl := !rtf;
          fl := fv;
          d)
        else (
          let d = !xh -. !rtf in
          xh := !rtf;
          fh := fv;
          d)
      in
      dx := !xh -. !xl;
      (if abs_float del < xacc || fv = 0.0 then res := Some (!rtf, 0));
      j := !j + 1
    done;
    (match !res with Some v -> v | None -> (!rtf, 1)))

let rtsec x1 x2 xacc =
  let fl = ref (func x1) and fv = ref (func x2) in
  let xl = ref 0.0 and rts = ref 0.0 in
  (if abs_float !fl < abs_float !fv then (
     rts := x1;
     xl := x2;
     let t = !fl in
     fl := !fv;
     fv := t)
   else (
     xl := x1;
     rts := x2));
  let res = ref None and j = ref 0 in
  while !j < 30 && !res = None do
    let dx = (!xl -. !rts) *. !fv /. (!fv -. !fl) in
    xl := !rts;
    fl := !fv;
    rts := !rts +. dx;
    fv := func !rts;
    (if abs_float dx < xacc || !fv = 0.0 then res := Some (!rts, 0));
    j := !j + 1
  done;
  (match !res with Some v -> v | None -> (!rts, 1))

let signb a b = abs_float a *. (if b >= 0.0 then 1.0 else -1.0)

let zriddr x1 x2 xacc =
  let fl = ref (func x1) and fh = ref (func x2) in
  if not ((!fl > 0.0 && !fh < 0.0) || (!fl < 0.0 && !fh > 0.0)) then
    if !fl = 0.0 then (x1, 0) else if !fh = 0.0 then (x2, 0) else (0.0, 1)
  else (
    let xl = ref x1 and xh = ref x2 and ans = ref (-9.99e99) in
    let res = ref None and j = ref 0 in
    while !j < 60 && !res = None do
      let xm = 0.5 *. (!xl +. !xh) in
      let fm = func xm in
      let s = sqrt (fm *. fm -. !fl *. !fh) in
      if s = 0.0 then res := Some (!ans, 0)
      else (
        let xnew = xm +. (xm -. !xl) *. ((if !fl >= !fh then 1.0 else -1.0) *. fm /. s) in
        if abs_float (xnew -. !ans) <= xacc then res := Some (!ans, 0)
        else (
          ans := xnew;
          let fnew = func !ans in
          if fnew = 0.0 then res := Some (!ans, 0)
          else (
            (if signb fm fnew <> fm then (
               xl := xm;
               fl := fm;
               xh := !ans;
               fh := fnew)
             else (
               let s2 = signb !fl fnew in
               if s2 <> !fl then (
                 xh := !ans;
                 fh := fnew)
               else (
                 xl := !ans;
                 fl := fnew)));
            (if abs_float (!xh -. !xl) <= xacc then res := Some (!ans, 0)))));
      j := !j + 1
    done;
    (match !res with Some v -> v | None -> (!ans, 1)))

let zbrent x1 x2 tol =
  let eps = 2.220446049250313e-16 in
  let a = ref x1 and b = ref x2 and c = ref x2 in
  let d = ref 0.0 and e = ref 0.0 in
  let fa = ref (func x1) and fb = ref (func x2) in
  if (!fa > 0.0 && !fb > 0.0) || (!fa < 0.0 && !fb < 0.0) then (0.0, 1)
  else (
    let fc = ref !fb in
    let res = ref None and iter = ref 0 in
    while !iter < 100 && !res = None do
      (if (!fb > 0.0 && !fc > 0.0) || (!fb < 0.0 && !fc < 0.0) then (
         c := !a;
         fc := !fa;
         e := !b -. !a;
         d := !e));
      (if abs_float !fc < abs_float !fb then (
         let ta = !a and tb = !b and tc = !c in
         a := tb;
         b := tc;
         c := ta;
         let tfa = !fa and tfb = !fb and tfc = !fc in
         fa := tfb;
         fb := tfc;
         fc := tfa));
      let tol1 = 2.0 *. eps *. abs_float !b +. 0.5 *. tol in
      let xm = 0.5 *. (!c -. !b) in
      if abs_float xm <= tol1 || !fb = 0.0 then res := Some (!b, 0)
      else (
        (if abs_float !e >= tol1 && abs_float !fa > abs_float !fb then (
           let s = !fb /. !fa in
           let p, q =
             if !a = !c then (2.0 *. xm *. s, 1.0 -. s)
             else (
               let q = !fa /. !fc and r = !fb /. !fc in
               ( s *. (2.0 *. xm *. q *. (q -. r) -. (!b -. !a) *. (r -. 1.0)),
                 (q -. 1.0) *. (r -. 1.0) *. (s -. 1.0) ))
           in
           let q = if p > 0.0 then -.q else q in
           let p = abs_float p in
           let min1 = 3.0 *. xm *. q -. abs_float (tol1 *. q)
           and min2 = abs_float (!e *. q) in
           if 2.0 *. p < (if min1 < min2 then min1 else min2) then (
             e := !d;
             d := p /. q)
           else (
             d := xm;
             e := !d))
         else (
           d := xm;
           e := !d));
        a := !b;
        fa := !fb;
        (if abs_float !d > tol1 then b := !b +. !d
         else b := !b +. abs_float tol1 *. (if xm >= 0.0 then 1.0 else -1.0));
        fb := func !b);
      iter := !iter + 1
    done;
    (match !res with Some v -> v | None -> (!b, 1)))

let rtnewt x1 x2 xacc =
  let rtn = ref (0.5 *. (x1 +. x2)) in
  let res = ref None and j = ref 0 in
  while !j < 20 && !res = None do
    let fv = func !rtn and dv = funcd !rtn in
    let dx = fv /. dv in
    rtn := !rtn -. dx;
    (if (x1 -. !rtn) *. (!rtn -. x2) < 0.0 then res := Some (0.0, 1)
     else if abs_float dx < xacc then res := Some (!rtn, 0));
    j := !j + 1
  done;
  (match !res with Some v -> v | None -> (!rtn, 1))

let rtsafe x1 x2 xacc =
  let fl = func x1 and fh = func x2 in
  if (fl > 0.0 && fh > 0.0) || (fl < 0.0 && fh < 0.0) then (0.0, 1)
  else if fl = 0.0 then (x1, 0)
  else if fh = 0.0 then (x2, 0)
  else (
    let xl = ref 0.0 and xh = ref 0.0 in
    (if fl < 0.0 then (
       xl := x1;
       xh := x2)
     else (
       xh := x1;
       xl := x2));
    let rts = ref (0.5 *. (x1 +. x2)) in
    let dxold = ref (abs_float (x2 -. x1)) and dx = ref (abs_float (x2 -. x1)) in
    let fv = ref (func !rts) and dv = ref (funcd !rts) in
    let res = ref None and j = ref 0 in
    while !j < 100 && !res = None do
      (if ((!rts -. !xh) *. !dv -. !fv) *. ((!rts -. !xl) *. !dv -. !fv) > 0.0
       || abs_float (2.0 *. !fv) > abs_float (!dxold *. !dv)
       then (
         dxold := !dx;
         dx := 0.5 *. (!xh -. !xl);
         rts := !xl +. !dx;
         if !xl = !rts then res := Some (!rts, 0))
       else (
         dxold := !dx;
         dx := !fv /. !dv;
         let temp = !rts in
         rts := !rts -. !dx;
         if temp = !rts then res := Some (!rts, 0)));
      (if !res = None then (
         if abs_float !dx < xacc then res := Some (!rts, 0)
         else (
           fv := func !rts;
           dv := funcd !rts;
           if !fv < 0.0 then xl := !rts else xh := !rts)));
      j := !j + 1
    done;
    (match !res with Some v -> v | None -> (!rts, 1)))

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
  let p = ref 2 in
  (if !fn = "POLY" then (
     let d = int_of_string toks.(2) in
     fp := Array.init (d + 1) (fun i -> float_of_string toks.(3 + i));
     p := 3 + d + 1)
   else if !fn = "POW" then (
     fp := Array.init 4 (fun i -> float_of_string toks.(2 + i));
     p := 6)
   else (
     fp := Array.init 3 (fun i -> float_of_string toks.(2 + i));
     p := 5));
  let x1 = float_of_string toks.(!p)
  and x2 = float_of_string toks.(!p + 1)
  and xacc = float_of_string toks.(!p + 2) in
  let ans, flag =
    if op = "BISECT" then rtbis x1 x2 xacc
    else if op = "SECANT" then rtsec x1 x2 xacc
    else if op = "FALSEPOS" then rtflsp x1 x2 xacc
    else if op = "RIDDER" then zriddr x1 x2 xacc
    else if op = "BRENT" then zbrent x1 x2 xacc
    else if op = "NEWTON" then rtnewt x1 x2 xacc
    else if op = "RTSAFE" then rtsafe x1 x2 xacc
    else exit 0
  in
  if flag <> 0 then print_endline "0.000000 1"
  else print_endline (fmt6 (canon ans) ^ " 0")
