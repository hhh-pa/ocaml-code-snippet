(* All eigenvalues and eigenvectors of a real symmetric
   matrix by Householder reduction to tridiagonal form
   followed by implicit-shift QL iteration (with eigsrt).
   Reference: Press et al., Numerical Recipes 3e, Sections 11.2-11.4.
   No online-judge problem found -- fixtures are self-generated
   (numpy oracle), no header link. *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

exception TooMany

let eigsrt d v =
  let n = Array.length d in
  for i = 0 to n - 2 do
    let k = ref i and p = ref d.(i) in
    for j = i to n - 1 do
      if d.(j) >= !p then (
        k := j;
        p := d.(j))
    done;
    if !k <> i then (
      d.(!k) <- d.(i);
      d.(i) <- !p;
      for j = 0 to n - 1 do
        let t = v.(j).(i) in
        v.(j).(i) <- v.(j).(!k);
        v.(j).(!k) <- t
      done)
  done

let pythag a b =
  let absa = abs_float a and absb = abs_float b in
  if absa > absb then absa *. sqrt (1.0 +. (absb /. absa) *. (absb /. absa))
  else if absb = 0.0 then 0.0
  else absb *. sqrt (1.0 +. (absa /. absb) *. (absa /. absb))

let sgn a b = if b >= 0.0 then abs_float a else -. abs_float a

let tred2 z d e =
  let n = Array.length z in
  for i = n - 1 downto 1 do
    let l = i - 1 in
    let h = ref 0.0 and scale = ref 0.0 in
    (if l > 0 then (
       for k = 0 to i - 1 do
         scale := !scale +. abs_float z.(i).(k)
       done;
       if !scale = 0.0 then e.(i) <- z.(i).(l)
       else (
         for k = 0 to i - 1 do
           z.(i).(k) <- z.(i).(k) /. !scale;
           h := !h +. z.(i).(k) *. z.(i).(k)
         done;
         let f = z.(i).(l) in
         let g = if f >= 0.0 then -. sqrt !h else sqrt !h in
         e.(i) <- !scale *. g;
         h := !h -. f *. g;
         z.(i).(l) <- f -. g;
         let f = ref 0.0 in
         for j = 0 to i - 1 do
           z.(j).(i) <- z.(i).(j) /. !h;
           let g = ref 0.0 in
           for k = 0 to j do
             g := !g +. z.(j).(k) *. z.(i).(k)
           done;
           for k = j + 1 to i - 1 do
             g := !g +. z.(k).(j) *. z.(i).(k)
           done;
           e.(j) <- !g /. !h;
           f := !f +. e.(j) *. z.(i).(j)
         done;
         let hh = !f /. (!h +. !h) in
         for j = 0 to i - 1 do
           let f = z.(i).(j) in
           let g = e.(j) -. hh *. f in
           e.(j) <- g;
           for k = 0 to j do
             z.(j).(k) <- z.(j).(k) -. (f *. e.(k) +. g *. z.(i).(k))
           done
         done))
     else e.(i) <- z.(i).(l));
    d.(i) <- !h
  done;
  d.(0) <- 0.0;
  e.(0) <- 0.0;
  for i = 0 to n - 1 do
    (if d.(i) <> 0.0 then
       for j = 0 to i - 1 do
         let g = ref 0.0 in
         for k = 0 to i - 1 do
           g := !g +. z.(i).(k) *. z.(k).(j)
         done;
         for k = 0 to i - 1 do
           z.(k).(j) <- z.(k).(j) -. !g *. z.(k).(i)
         done
       done);
    d.(i) <- z.(i).(i);
    z.(i).(i) <- 1.0;
    for j = 0 to i - 1 do
      z.(j).(i) <- 0.0;
      z.(i).(j) <- 0.0
    done
  done

let tqli d e z =
  let eps = 2.220446049250313e-16 in
  let n = Array.length d in
  for i = 1 to n - 1 do
    e.(i - 1) <- e.(i)
  done;
  e.(n - 1) <- 0.0;
  for l = 0 to n - 1 do
    let iter = ref 0 in
    let m = ref l in
    let again = ref true in
    while !again do
      m := l;
      while !m < n - 1
            && not (abs_float e.(!m) <= eps *. (abs_float d.(!m) +. abs_float d.(!m + 1))) do
        incr m
      done;
      if !m = l then again := false
      else (
        if !iter = 30 then raise TooMany;
        incr iter;
        let g0 = (d.(l + 1) -. d.(l)) /. (2.0 *. e.(l)) in
        let r0 = pythag g0 1.0 in
        let g0 = d.(!m) -. d.(l) +. e.(l) /. (g0 +. sgn r0 g0) in
        let s = ref 1.0 and c = ref 1.0 and p = ref 0.0 in
        let g = ref g0 and r = ref r0 and i = ref (!m - 1) in
        let broke = ref false in
        while !i >= l && not !broke do
          let f = !s *. e.(!i) in
          let b = !c *. e.(!i) in
          r := pythag f !g;
          e.(!i + 1) <- !r;
          if !r = 0.0 then (
            d.(!i + 1) <- d.(!i + 1) -. !p;
            e.(!m) <- 0.0;
            broke := true)
          else (
            s := f /. !r;
            c := !g /. !r;
            g := d.(!i + 1) -. !p;
            r := (d.(!i) -. !g) *. !s +. 2.0 *. !c *. b;
            p := !s *. !r;
            d.(!i + 1) <- !g +. !p;
            g := !c *. !r -. b;
            for k = 0 to n - 1 do
              let f = z.(k).(!i + 1) in
              z.(k).(!i + 1) <- !s *. z.(k).(!i) +. !c *. f;
              z.(k).(!i) <- !c *. z.(k).(!i) -. !s *. f
            done;
            decr i)
        done;
        if !r = 0.0 && !i >= l then ()
        else (
          d.(l) <- d.(l) -. !p;
          e.(l) <- !g;
          e.(!m) <- 0.0))
    done
  done

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
  let n = int_of_string toks.(1) in
  let p = ref 2 in
  if op = "SYMEIG" || op = "TQLI" then
    try
      let d, z =
        if op = "SYMEIG" then (
          let a = Array.make_matrix n n 0.0 in
          for i = 0 to n - 1 do
            for j = 0 to n - 1 do
              a.(i).(j) <- float_of_string toks.(!p);
              incr p
            done
          done;
          let d = Array.make n 0.0 and e = Array.make n 0.0 in
          tred2 a d e;
          tqli d e a;
          eigsrt d a;
          (d, a))
        else (
          let dd = Array.init n (fun _ ->
            let v = float_of_string toks.(!p) in
            incr p;
            v) in
          let ee = Array.init n (fun _ ->
            let v = float_of_string toks.(!p) in
            incr p;
            v) in
          let z = Array.make_matrix n n 0.0 in
          for i = 0 to n - 1 do
            z.(i).(i) <- 1.0
          done;
          tqli dd ee z;
          eigsrt dd z;
          (dd, z))
      in
      let parts = ref [] in
      for i = 0 to n - 1 do
        parts := fmt6 (canon d.(i)) :: !parts
      done;
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          parts := fmt6 (canon z.(i).(j)) :: !parts
        done
      done;
      parts := "0" :: !parts;
      print_endline (String.concat " " (List.rev !parts))
    with TooMany ->
      let parts = Array.to_list (Array.init (n + n * n) (fun _ -> "0.000000")) in
      print_endline (String.concat " " parts ^ " 1")
