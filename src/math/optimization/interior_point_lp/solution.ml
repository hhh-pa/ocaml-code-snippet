(* Linear programming by the book's basic primal-dual
   infeasible interior-point method (routine intpt; dense
   LU substitutes the book's external sparse LDL+AMD
   backend, a Webnote-only performance optimization).
   Reference: Press et al., Numerical Recipes 3e, Section 10.11.
   No online-judge problem found -- fixtures are self-generated
   (scipy oracle), no header link. *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

exception Singular

let dotprod x y =
  let s = ref 0.0 in
  for i = 0 to Array.length x - 1 do
    s := !s +. x.(i) *. y.(i)
  done;
  !s

type lu_t = { ln : int; lu : float array array; indx : int array }

let new_lu a =
  let n = Array.length a in
  let lu = Array.map Array.copy a in
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

let lu_solve l b x =
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

let intpt a b c x =
  let maxits = 200 in
  let eps = 1.0e-6 in
  let sigma = 0.9 in
  let delta = 0.02 in
  let big = 1.7976931348623157e308 in
  let m = Array.length a in
  let n = Array.length a.(0) in
  let y = Array.make m 0.0
  and z = Array.make n 0.0
  and ax = Array.make m 0.0
  and aty = Array.make n 0.0
  and rp = Array.make m 0.0
  and rd = Array.make n 0.0
  and d = Array.make n 0.0
  and dx = Array.make n 0.0
  and dy = Array.make m 0.0
  and dz = Array.make n 0.0
  and rhs = Array.make m 0.0
  and tempm = Array.make m 0.0
  and tempn = Array.make n 0.0 in
  let at = Array.make_matrix n m 0.0 in
  for i = 0 to m - 1 do
    for j = 0 to n - 1 do
      at.(j).(i) <- a.(i).(j)
    done
  done;
  let rpfact = 1.0 +. sqrt (dotprod b b) in
  let rdfact = 1.0 +. sqrt (dotprod c c) in
  for j = 0 to n - 1 do
    x.(j) <- 1000.0;
    z.(j) <- 1000.0
  done;
  for i = 0 to m - 1 do
    y.(i) <- 1000.0
  done;
  let mat = Array.make_matrix m m 0.0 in
  let rec loop iter normrp_old normrd_old =
    if iter >= maxits then 3
    else (
      for i = 0 to m - 1 do
        let s = ref 0.0 in
        for j = 0 to n - 1 do
          s := !s +. a.(i).(j) *. x.(j)
        done;
        ax.(i) <- !s
      done;
      for i = 0 to m - 1 do
        rp.(i) <- ax.(i) -. b.(i)
      done;
      let normrp = sqrt (dotprod rp rp) /. rpfact in
      for j = 0 to n - 1 do
        let s = ref 0.0 in
        for i = 0 to m - 1 do
          s := !s +. at.(j).(i) *. y.(i)
        done;
        aty.(j) <- !s
      done;
      for j = 0 to n - 1 do
        rd.(j) <- aty.(j) +. z.(j) -. c.(j)
      done;
      let normrd = sqrt (dotprod rd rd) /. rdfact in
      let gamma = dotprod x z in
      let mu = delta *. gamma /. float n in
      let primal_obj = dotprod c x in
      let gamma_norm = gamma /. (1.0 +. abs_float primal_obj) in
      if normrp < eps && normrd < eps && gamma_norm < eps then 0
      else if normrp > 1000.0 *. normrp_old && normrp > eps then 1
      else if normrd > 1000.0 *. normrd_old && normrd > eps then 2
      else (
        for j = 0 to n - 1 do
          d.(j) <- x.(j) /. z.(j)
        done;
        for i = 0 to m - 1 do
          for k = 0 to m - 1 do
            let s = ref 0.0 in
            for j = 0 to n - 1 do
              s := !s +. a.(i).(j) *. d.(j) *. a.(k).(j)
            done;
            mat.(i).(k) <- !s
          done
        done;
        for j = 0 to n - 1 do
          tempn.(j) <- x.(j) -. mu /. z.(j) -. d.(j) *. rd.(j)
        done;
        for i = 0 to m - 1 do
          let s = ref 0.0 in
          for j = 0 to n - 1 do
            s := !s +. a.(i).(j) *. tempn.(j)
          done;
          tempm.(i) <- !s
        done;
        for i = 0 to m - 1 do
          rhs.(i) <- -. rp.(i) +. tempm.(i)
        done;
        (try
           let alu = new_lu mat in
           lu_solve alu rhs dy
         with Singular -> raise Exit);
        for j = 0 to n - 1 do
          let s = ref 0.0 in
          for i = 0 to m - 1 do
            s := !s +. at.(j).(i) *. dy.(i)
          done;
          tempn.(j) <- !s
        done;
        for j = 0 to n - 1 do
          dz.(j) <- -. tempn.(j) -. rd.(j)
        done;
        for j = 0 to n - 1 do
          dx.(j) <- -. d.(j) *. dz.(j) +. mu /. z.(j) -. x.(j)
        done;
        let alpha_p = ref 1.0 in
        for j = 0 to n - 1 do
          if x.(j) +. !alpha_p *. dx.(j) < 0.0 then alpha_p := -. x.(j) /. dx.(j)
        done;
        let alpha_d = ref 1.0 in
        for j = 0 to n - 1 do
          if z.(j) +. !alpha_d *. dz.(j) < 0.0 then alpha_d := -. z.(j) /. dz.(j)
        done;
        let ap = min (!alpha_p *. sigma) 1.0 and ad = min (!alpha_d *. sigma) 1.0 in
        for j = 0 to n - 1 do
          x.(j) <- x.(j) +. ap *. dx.(j);
          z.(j) <- z.(j) +. ad *. dz.(j)
        done;
        for i = 0 to m - 1 do
          y.(i) <- y.(i) +. ad *. dy.(i)
        done;
        loop (iter + 1) normrp normrd))
  in
  try loop 0 big big with Exit -> 3

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
  let m = int_of_string toks.(1) and n = int_of_string toks.(2) in
  let p = ref 3 in
  let a = Array.make_matrix m n 0.0 in
  for i = 0 to m - 1 do
    for j = 0 to n - 1 do
      a.(i).(j) <- float_of_string toks.(!p);
      incr p
    done
  done;
  let b = Array.init m (fun _ ->
    let v = float_of_string toks.(!p) in
    incr p;
    v) in
  let c = Array.init n (fun _ ->
    let v = float_of_string toks.(!p) in
    incr p;
    v) in
  if op = "INTPT" then (
    let x = Array.make n 0.0 in
    let status = intpt a b c x in
    let parts = Array.to_list (Array.init n (fun j -> fmt6 (canon x.(j)))) in
    print_endline (String.concat " " parts ^ " " ^ string_of_int status))
