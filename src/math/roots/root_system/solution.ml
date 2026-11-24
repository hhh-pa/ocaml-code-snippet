(* Nonlinear systems (NR 9.6-9.7): Newton (mnewt/newt),
   Broyden with QR updates, backtracking lnsrch;
   LU (2.3) and QR (2.10) engines vendored.
   Reference: Press et al., Numerical Recipes 3e, Sections 9.6-9.7.
   No online-judge problem found -- fixtures are self-generated
   (scipy oracle), no header link. *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

let sysid = ref ""

type vec = float array
type mat = float array array

let mat_copy (a : mat) : mat = Array.map Array.copy a

let vecfunc (x : vec) : vec =
  let n = Array.length x in
  let f = Array.make n 0.0 in
  if !sysid = "SYS1" then (
    f.(0) <- x.(0) *. x.(0) +. x.(1) *. x.(1) -. 4.0;
    f.(1) <- x.(0) *. x.(1) -. 1.0)
  else if !sysid = "SYS2" then (
    f.(0) <- x.(0) *. x.(0) -. x.(1) -. 1.0;
    f.(1) <- x.(0) -. x.(1) *. x.(1) +. 1.0)
  else (
    f.(0) <- x.(0) *. x.(0) +. x.(1) *. x.(1) +. x.(2) *. x.(2) -. 14.0;
    f.(1) <- x.(0) *. x.(1) *. x.(2) -. 6.0;
    f.(2) <- x.(0) +. x.(1) +. x.(2) -. 6.0);
  f

let jacfunc (x : vec) : mat =
  let n = Array.length x in
  let j = Array.make_matrix n n 0.0 in
  if !sysid = "SYS1" then (
    j.(0).(0) <- 2.0 *. x.(0);
    j.(0).(1) <- 2.0 *. x.(1);
    j.(1).(0) <- x.(1);
    j.(1).(1) <- x.(0))
  else if !sysid = "SYS2" then (
    j.(0).(0) <- 2.0 *. x.(0);
    j.(0).(1) <- -1.0;
    j.(1).(0) <- 1.0;
    j.(1).(1) <- -2.0 *. x.(1))
  else (
    j.(0).(0) <- 2.0 *. x.(0);
    j.(0).(1) <- 2.0 *. x.(1);
    j.(0).(2) <- 2.0 *. x.(2);
    j.(1).(0) <- x.(1) *. x.(2);
    j.(1).(1) <- x.(0) *. x.(2);
    j.(1).(2) <- x.(0) *. x.(1);
    j.(2).(0) <- 1.0;
    j.(2).(1) <- 1.0;
    j.(2).(2) <- 1.0);
  j

exception Singular

type lu_t = { ln : int; lu : mat; indx : int array }

let new_lu (a : mat) : lu_t =
  let n = Array.length a in
  let lu = mat_copy a in
  let indx = Array.make n 0 in
  let tiny = 1.0e-40 in
  let vv = Array.make n 0.0 in
  for i = 0 to n - 1 do
    let big = ref 0.0 in
    for j = 0 to n - 1 do
      let temp = abs_float lu.(i).(j) in
      if temp > !big then big := temp
    done;
    if !big = 0.0 then raise Singular;
    vv.(i) <- 1.0 /. !big
  done;
  for k = 0 to n - 1 do
    let big = ref 0.0 in
    let imax = ref k in
    for i = k to n - 1 do
      let temp = vv.(i) *. abs_float lu.(i).(k) in
      if temp > !big then (big := temp; imax := i)
    done;
    if k <> !imax then (
      for j = 0 to n - 1 do
        let temp = lu.(!imax).(j) in
        lu.(!imax).(j) <- lu.(k).(j);
        lu.(k).(j) <- temp
      done;
      vv.(!imax) <- vv.(k));
    indx.(k) <- !imax;
    if lu.(k).(k) = 0.0 then lu.(k).(k) <- tiny;
    for i = k + 1 to n - 1 do
      lu.(i).(k) <- lu.(i).(k) /. lu.(k).(k);
      let temp = lu.(i).(k) in
      for j = k + 1 to n - 1 do
        lu.(i).(j) <- lu.(i).(j) -. temp *. lu.(k).(j)
      done
    done
  done;
  { ln = n; lu; indx }

let lu_solve (l : lu_t) (b : vec) (x : vec) : unit =
  let n = l.ln in
  Array.blit b 0 x 0 n;
  let ii = ref 0 in
  for i = 0 to n - 1 do
    let ip = l.indx.(i) in
    let sum = ref x.(ip) in
    x.(ip) <- x.(i);
    if !ii <> 0 then
      for j = !ii - 1 to i - 1 do
        sum := !sum -. l.lu.(i).(j) *. x.(j)
      done
    else if !sum <> 0.0 then ii := i + 1;
    x.(i) <- !sum
  done;
  for i = n - 1 downto 0 do
    let sum = ref x.(i) in
    for j = i + 1 to n - 1 do
      sum := !sum -. l.lu.(i).(j) *. x.(j)
    done;
    x.(i) <- !sum /. l.lu.(i).(i)
  done

type qr_t = { qn : int; qt : mat; r : mat; c : vec; d : vec; mutable sing : bool }

let new_qr (a : mat) : qr_t =
  let n = Array.length a in
  let qt = Array.make_matrix n n 0.0 in
  let r = mat_copy a in
  let c = Array.make n 0.0 in
  let d = Array.make n 0.0 in
  let q = { qn = n; qt; r; c; d; sing = false } in
  for k = 0 to n - 2 do
    let scale = ref 0.0 in
    for i = k to n - 1 do
      scale := max !scale (abs_float r.(i).(k))
    done;
    if !scale = 0.0 then (
      q.sing <- true;
      c.(k) <- 0.0;
      d.(k) <- 0.0)
    else (
      for i = k to n - 1 do
        r.(i).(k) <- r.(i).(k) /. !scale
      done;
      let sum = ref 0.0 in
      for i = k to n - 1 do
        sum := !sum +. r.(i).(k) *. r.(i).(k)
      done;
      let sigma =
        if r.(k).(k) >= 0.0 then abs_float (sqrt !sum) else -. abs_float (sqrt !sum)
      in
      r.(k).(k) <- r.(k).(k) +. sigma;
      c.(k) <- sigma *. r.(k).(k);
      d.(k) <- -. !scale *. sigma;
      for j = k + 1 to n - 1 do
        let s = ref 0.0 in
        for i = k to n - 1 do
          s := !s +. r.(i).(k) *. r.(i).(j)
        done;
        let tau = !s /. c.(k) in
        for i = k to n - 1 do
          r.(i).(j) <- r.(i).(j) -. tau *. r.(i).(k)
        done
      done)
  done;
  d.(n - 1) <- r.(n - 1).(n - 1);
  if d.(n - 1) = 0.0 then q.sing <- true;
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      qt.(i).(j) <- (if i = j then 1.0 else 0.0)
    done
  done;
  for k = 0 to n - 2 do
    if c.(k) <> 0.0 then
      for j = 0 to n - 1 do
        let sum = ref 0.0 in
        for i = k to n - 1 do
          sum := !sum +. r.(i).(k) *. qt.(i).(j)
        done;
        sum := !sum /. c.(k);
        for i = k to n - 1 do
          qt.(i).(j) <- qt.(i).(j) -. !sum *. r.(i).(k)
        done
      done
  done;
  for i = 0 to n - 1 do
    r.(i).(i) <- d.(i);
    for j = 0 to i - 1 do
      r.(i).(j) <- 0.0
    done
  done;
  q

let qr_rotate (q : qr_t) (i : int) (a : float) (b : float) : unit =
  let n = q.qn and r = q.r and qt = q.qt in
  let cc, s =
    if a = 0.0 then (0.0, if b >= 0.0 then 1.0 else -1.0)
    else if abs_float a > abs_float b then (
      let fact = b /. a in
      let v = abs_float (1.0 /. sqrt (1.0 +. fact *. fact)) in
      let cc = if a >= 0.0 then v else -. v in
      (cc, fact *. cc))
    else (
      let fact = a /. b in
      let v = abs_float (1.0 /. sqrt (1.0 +. fact *. fact)) in
      let s = if b >= 0.0 then v else -. v in
      (fact *. s, s))
  in
  for j = i to n - 1 do
    let y = r.(i).(j) and w = r.(i + 1).(j) in
    r.(i).(j) <- cc *. y -. s *. w;
    r.(i + 1).(j) <- s *. y +. cc *. w
  done;
  for j = 0 to n - 1 do
    let y = qt.(i).(j) and w = qt.(i + 1).(j) in
    qt.(i).(j) <- cc *. y -. s *. w;
    qt.(i + 1).(j) <- s *. y +. cc *. w
  done

let qr_qtmult (q : qr_t) (b : vec) (x : vec) : unit =
  let n = q.qn in
  for i = 0 to n - 1 do
    let sum = ref 0.0 in
    for j = 0 to n - 1 do
      sum := !sum +. q.qt.(i).(j) *. b.(j)
    done;
    x.(i) <- !sum
  done

let qr_rsolve (q : qr_t) (b : vec) (x : vec) : unit =
  let n = q.qn in
  Array.blit b 0 x 0 n;
  for i = n - 1 downto 0 do
    let sum = ref x.(i) in
    for j = i + 1 to n - 1 do
      sum := !sum -. q.r.(i).(j) *. x.(j)
    done;
    x.(i) <- !sum /. q.r.(i).(i)
  done

let qr_update (q : qr_t) (u : vec) (v : vec) : unit =
  let n = q.qn and r = q.r in
  let w = Array.copy u in
  let k = ref (-1) in
  let i = ref (n - 1) in
  while !i >= 0 && !k < 0 do
    if w.(!i) <> 0.0 then k := !i;
    decr i
  done;
  if !k < 0 then k := 0;
  for i = !k - 1 downto 0 do
    qr_rotate q i w.(i) (-. w.(i + 1));
    if w.(i) = 0.0 then w.(i) <- abs_float w.(i + 1)
    else if abs_float w.(i) > abs_float w.(i + 1) then (
      let t = w.(i + 1) /. w.(i) in
      w.(i) <- abs_float w.(i) *. sqrt (1.0 +. t *. t))
    else (
      let t = w.(i) /. w.(i + 1) in
      w.(i) <- abs_float w.(i + 1) *. sqrt (1.0 +. t *. t))
  done;
  for i = 0 to n - 1 do
    r.(0).(i) <- r.(0).(i) +. w.(0) *. v.(i)
  done;
  for i = 0 to !k - 1 do
    qr_rotate q i r.(i).(i) (-. r.(i + 1).(i))
  done;
  for i = 0 to n - 1 do
    if r.(i).(i) = 0.0 then q.sing <- true
  done

let fdjac (x : vec) (fvec : vec) : mat =
  let eps = 1.0e-8 in
  let n = Array.length x in
  let df = Array.make_matrix n n 0.0 in
  let xh = Array.copy x in
  for j = 0 to n - 1 do
    let temp = xh.(j) in
    let h0 = eps *. abs_float temp in
    let h1 = if h0 = 0.0 then eps else h0 in
    xh.(j) <- temp +. h1;
    let h = xh.(j) -. temp in
    let f = vecfunc xh in
    xh.(j) <- temp;
    for i = 0 to n - 1 do
      df.(i).(j) <- (f.(i) -. fvec.(i)) /. h
    done
  done;
  df

let fminfn (x : vec) (fvec : vec) : float =
  let f = vecfunc x in
  let n = Array.length x in
  Array.blit f 0 fvec 0 n;
  let sum = ref 0.0 in
  for i = 0 to n - 1 do
    sum := !sum +. fvec.(i) *. fvec.(i)
  done;
  0.5 *. !sum

let maxabs (v : vec) : float =
  let t = ref 0.0 in
  for i = 0 to Array.length v - 1 do
    let a = abs_float v.(i) in
    if a > !t then t := a
  done;
  !t

let mnewt (x : vec) (ntrial : int) (tolx : float) (tolf : float) : unit =
  let n = Array.length x in
  let p = Array.make n 0.0 and fvec = Array.make n 0.0 in
  let k = ref 0 in
  let stop = ref false in
  while !k < ntrial && not !stop do
    Array.blit (vecfunc x) 0 fvec 0 n;
    let fjac = jacfunc x in
    let errf = ref 0.0 in
    for i = 0 to n - 1 do
      errf := !errf +. abs_float fvec.(i)
    done;
    if !errf <= tolf then stop := true
    else (
      for i = 0 to n - 1 do
        p.(i) <- -. fvec.(i)
      done;
      let alu = new_lu fjac in
      lu_solve alu p p;
      let errx = ref 0.0 in
      for i = 0 to n - 1 do
        errx := !errx +. abs_float p.(i);
        x.(i) <- x.(i) +. p.(i)
      done;
      if !errx <= tolx then stop := true);
    incr k
  done

let lnsrch (xold : vec) (fold : float) (g : vec) (p : vec) (x : vec) (stpmax : float) :
    float * bool * int =
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
  if !slope >= 0.0 then (0.0, false, 1)
  else (
    let test = ref 0.0 in
    for i = 0 to n - 1 do
      let temp = abs_float p.(i) /. max (abs_float xold.(i)) 1.0 in
      if temp > !test then test := temp
    done;
    let alamin = tolx /. !test in
    let fvec = Array.make n 0.0 in
    let alam = ref 1.0 and alam2 = ref 0.0 and f2 = ref 0.0 in
    let res = ref None in
    while !res = None do
      for i = 0 to n - 1 do
        x.(i) <- xold.(i) +. !alam *. p.(i)
      done;
      let f = fminfn x fvec in
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
            if a = 0.0 then (-. !slope) /. (2.0 *. b)
            else (
              let disc = b *. b -. 3.0 *. a *. !slope in
              if disc < 0.0 then 0.5 *. !alam
              else if b <= 0.0 then (-. b +. sqrt disc) /. (3.0 *. a)
              else (-. !slope) /. (b +. sqrt disc)))
        in
        let tmplam = if tmplam > 0.5 *. !alam then 0.5 *. !alam else tmplam in
        alam2 := !alam;
        f2 := f;
        alam := max tmplam (0.1 *. !alam))
    done;
    match !res with Some v -> v | None -> assert false)

let newt (x : vec) : bool * int =
  let maxits = 200 in
  let tolf = 1.0e-8 and tolmin = 1.0e-12 and stpmx = 100.0 in
  let tolx = 2.220446049250313e-16 in
  let n = Array.length x in
  let g = Array.make n 0.0
  and p = Array.make n 0.0
  and xold = Array.make n 0.0
  and fvec = Array.make n 0.0 in
  let f0 = fminfn x fvec in
  if maxabs fvec < 0.01 *. tolf then (false, 0)
  else (
    let ss = ref 0.0 in
    for i = 0 to n - 1 do
      ss := !ss +. x.(i) *. x.(i)
    done;
    let stpmax = stpmx *. max (sqrt !ss) (float n) in
    let rec loop its f check =
      if its >= maxits then (check, 1)
      else (
        let fjac = fdjac x fvec in
        for i = 0 to n - 1 do
          let s = ref 0.0 in
          for j = 0 to n - 1 do
            s := !s +. fjac.(j).(i) *. fvec.(j)
          done;
          g.(i) <- !s
        done;
        Array.blit x 0 xold 0 n;
        let fold = f in
        for i = 0 to n - 1 do
          p.(i) <- -. fvec.(i)
        done;
        (try
           let alu = new_lu fjac in
           lu_solve alu p p;
           let nf, ncheck, bad = lnsrch xold fold g p x stpmax in
           if bad <> 0 then (ncheck, 1)
           else (
             Array.blit (vecfunc x) 0 fvec 0 n;
             if maxabs fvec < tolf then (false, 0)
             else if ncheck then (
               let den = max nf (0.5 *. float n) in
               let t = ref 0.0 in
               for i = 0 to n - 1 do
                 let temp = abs_float g.(i) *. max (abs_float x.(i)) 1.0 /. den in
                 if temp > !t then t := temp
               done;
               (!t < tolmin, 0))
             else (
               let t = ref 0.0 in
               for i = 0 to n - 1 do
                 let temp =
                   abs_float (x.(i) -. xold.(i)) /. max (abs_float x.(i)) 1.0
                 in
                 if temp > !t then t := temp
               done;
               if !t < tolx then (ncheck, 0) else loop (its + 1) nf ncheck))
        with Singular -> (check, 1)))
    in
    loop 0 f0 false)

let broydn (x : vec) : bool * int =
  let maxits = 200 in
  let eps = 2.220446049250313e-16 in
  let tolf = 1.0e-8 and tolx = eps and stpmx = 100.0 and tolmin = 1.0e-12 in
  let n = Array.length x in
  let fvcold = Array.make n 0.0
  and g = Array.make n 0.0
  and p = Array.make n 0.0
  and s = Array.make n 0.0
  and t = Array.make n 0.0
  and w = Array.make n 0.0
  and xold = Array.make n 0.0
  and fvec = Array.make n 0.0 in
  let f0 = fminfn x fvec in
  if maxabs fvec < 0.01 *. tolf then (false, 0)
  else (
    let ss = ref 0.0 in
    for i = 0 to n - 1 do
      ss := !ss +. x.(i) *. x.(i)
    done;
    let stpmax = stpmx *. max (sqrt !ss) (float n) in
    let rec loop its f check restrt qr flag =
      if its > maxits then (check, flag)
      else (
        let qr2, failed =
          if restrt then (
            let q = new_qr (fdjac x fvec) in
            (q, q.sing))
          else (
            let q = match qr with Some q -> q | None -> assert false in
            for i = 0 to n - 1 do
              s.(i) <- x.(i) -. xold.(i)
            done;
            for i = 0 to n - 1 do
              let sm = ref 0.0 in
              for j = i to n - 1 do
                sm := !sm +. q.r.(i).(j) *. s.(j)
              done;
              t.(i) <- !sm
            done;
            let skip = ref true in
            for i = 0 to n - 1 do
              let sm = ref 0.0 in
              for j = 0 to n - 1 do
                sm := !sm +. q.qt.(j).(i) *. t.(j)
              done;
              w.(i) <- fvec.(i) -. fvcold.(i) -. !sm;
              if abs_float w.(i) >= eps *. (abs_float fvec.(i) +. abs_float fvcold.(i)) then
                skip := false
              else w.(i) <- 0.0
            done;
            if !skip then (q, false)
            else (
              qr_qtmult q w t;
              let den = ref 0.0 in
              for i = 0 to n - 1 do
                den := !den +. s.(i) *. s.(i)
              done;
              for i = 0 to n - 1 do
                s.(i) <- s.(i) /. !den
              done;
              qr_update q t s;
              (q, q.sing)))
        in
        if failed then (check, 1)
        else (
          qr_qtmult qr2 fvec p;
          for i = 0 to n - 1 do
            p.(i) <- -. p.(i)
          done;
          for i = n - 1 downto 0 do
            let sm = ref 0.0 in
            for j = 0 to i do
              sm := !sm -. qr2.r.(j).(i) *. p.(j)
            done;
            g.(i) <- !sm
          done;
          Array.blit x 0 xold 0 n;
          Array.blit fvec 0 fvcold 0 n;
          let fold = f in
          qr_rsolve qr2 p p;
          let nf, ncheck, bad = lnsrch xold fold g p x stpmax in
          if bad <> 0 then (ncheck, 1)
          else (
            Array.blit (vecfunc x) 0 fvec 0 n;
            if maxabs fvec < tolf then (false, flag)
            else if ncheck then
              if restrt then (ncheck, flag)
              else (
                let den = max nf (0.5 *. float n) in
                let t2 = ref 0.0 in
                for i = 0 to n - 1 do
                  let temp = abs_float g.(i) *. max (abs_float x.(i)) 1.0 /. den in
                  if temp > !t2 then t2 := temp
                done;
                if !t2 < tolmin then (ncheck, flag)
                else loop (its + 1) nf ncheck true (Some qr2) (if its = maxits then 1 else flag))
            else (
              let t3 = ref 0.0 in
              for i = 0 to n - 1 do
                let temp =
                  abs_float (x.(i) -. xold.(i)) /. max (abs_float x.(i)) 1.0
                in
                if temp > !t3 then t3 := temp
              done;
              if !t3 < tolx then (ncheck, flag)
              else loop (its + 1) nf ncheck false (Some qr2) (if its = maxits then 1 else flag)))))
    in
    loop 1 f0 false true None 0)

let print_x (x : vec) (zero : bool) : string =
  String.concat " "
    (Array.to_list
       (Array.init (Array.length x) (fun i -> fmt6 (canon (if zero then 0.0 else x.(i))))))

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
  sysid := toks.(1);
  let n = if !sysid = "SYS3" then 3 else 2 in
  let x = Array.init n (fun i -> float_of_string toks.(2 + i)) in
  let p0 = 2 + n in
  if op = "MNEWT" then (
    let ntrial = int_of_string toks.(p0)
    and tolx = float_of_string toks.(p0 + 1)
    and tolf = float_of_string toks.(p0 + 2) in
    (try
       mnewt x ntrial tolx tolf;
       print_endline (print_x x false ^ " 0")
     with Singular -> print_endline (print_x x true ^ " 1")))
  else if op = "NEWT" || op = "BROYDN" then (
    let check, flag = if op = "NEWT" then newt x else broydn x in
    print_endline
      (print_x x (flag <> 0) ^ " " ^ (if check then "1" else "0") ^ " " ^ string_of_int flag))
  else if op = "LNSRCH" then (
    let pp = Array.init n (fun i -> float_of_string toks.(p0 + i)) in
    let stpmax = float_of_string toks.(p0 + n) in
    let fvec = Array.make n 0.0 in
    let fold = fminfn x fvec in
    let fj = fdjac x fvec in
    let g = Array.make n 0.0 and xn = Array.make n 0.0 in
    for i = 0 to n - 1 do
      let s = ref 0.0 in
      for j = 0 to n - 1 do
        s := !s +. fj.(j).(i) *. fvec.(j)
      done;
      g.(i) <- !s
    done;
    let f, check, bad = lnsrch x fold g pp xn stpmax in
    if bad <> 0 then print_endline (print_x x false ^ " " ^ fmt6 (canon fold) ^ " 1")
    else
      print_endline
        (print_x xn false ^ " " ^ fmt6 (canon f) ^ " " ^ (if check then "1" else "0")))
