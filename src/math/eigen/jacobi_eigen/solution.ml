(* All eigenvalues and eigenvectors of a real symmetric
   matrix by Jacobi's cyclic method (with eigsrt).
   Reference: Press et al., Numerical Recipes 3e, Section 11.1.
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

let rot a s tau i j k l =
  let g = a.(i).(j) and h = a.(k).(l) in
  a.(i).(j) <- g -. s *. (h +. g *. tau);
  a.(k).(l) <- h +. s *. (g -. h *. tau)

let jacobi aa =
  let eps = 2.220446049250313e-16 in
  let n = Array.length aa in
  let a = Array.map Array.copy aa in
  let v = Array.make_matrix n n 0.0 in
  let d = Array.make n 0.0 in
  let nrot = ref 0 in
  let b = Array.make n 0.0 and z = Array.make n 0.0 in
  for ip = 0 to n - 1 do
    for iq = 0 to n - 1 do
      v.(ip).(iq) <- 0.0
    done;
    v.(ip).(ip) <- 1.0
  done;
  for ip = 0 to n - 1 do
    b.(ip) <- a.(ip).(ip);
    d.(ip) <- b.(ip);
    z.(ip) <- 0.0
  done;
  let rec sweep i =
    if i > 50 then raise TooMany
    else (
      let sm = ref 0.0 in
      for ip = 0 to n - 2 do
        for iq = ip + 1 to n - 1 do
          sm := !sm +. abs_float a.(ip).(iq)
        done
      done;
      if !sm = 0.0 then (
        eigsrt d v;
        (d, v, !nrot))
      else (
        let tresh = if i < 4 then 0.2 *. !sm /. float (n * n) else 0.0 in
        for ip = 0 to n - 2 do
          for iq = ip + 1 to n - 1 do
            let g = 100.0 *. abs_float a.(ip).(iq) in
            if i > 4 && g <= eps *. abs_float d.(ip) && g <= eps *. abs_float d.(iq) then
              a.(ip).(iq) <- 0.0
            else if abs_float a.(ip).(iq) > tresh then (
              let h = d.(iq) -. d.(ip) in
              let t =
                if g <= eps *. abs_float h then a.(ip).(iq) /. h
                else (
                  let theta = 0.5 *. h /. a.(ip).(iq) in
                  let tt = 1.0 /. (abs_float theta +. sqrt (1.0 +. theta *. theta)) in
                  if theta < 0.0 then -. tt else tt)
              in
              let c = 1.0 /. sqrt (1.0 +. t *. t) in
              let s = t *. c in
              let tau = s /. (1.0 +. c) in
              let h = t *. a.(ip).(iq) in
              z.(ip) <- z.(ip) -. h;
              z.(iq) <- z.(iq) +. h;
              d.(ip) <- d.(ip) -. h;
              d.(iq) <- d.(iq) +. h;
              a.(ip).(iq) <- 0.0;
              for j = 0 to ip - 1 do
                rot a s tau j ip j iq
              done;
              for j = ip + 1 to iq - 1 do
                rot a s tau ip j j iq
              done;
              for j = iq + 1 to n - 1 do
                rot a s tau ip j iq j
              done;
              for j = 0 to n - 1 do
                rot v s tau j ip j iq
              done;
              incr nrot)
          done
        done;
        for ip = 0 to n - 1 do
          b.(ip) <- b.(ip) +. z.(ip);
          d.(ip) <- b.(ip);
          z.(ip) <- 0.0
        done;
        sweep (i + 1)))
  in
  sweep 1

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
  let a = Array.make_matrix n n 0.0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      a.(i).(j) <- float_of_string toks.(!p);
      incr p
    done
  done;
  if op = "JACOBI" then
    try
      let d, v, nrot = jacobi a in
      let parts = ref [] in
      for i = 0 to n - 1 do
        parts := fmt6 (canon d.(i)) :: !parts
      done;
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          parts := fmt6 (canon v.(i).(j)) :: !parts
        done
      done;
      parts := "0" :: string_of_int nrot :: !parts;
      print_endline (String.concat " " (List.rev !parts))
    with TooMany ->
      let parts = Array.to_list (Array.init (n + n * n) (fun _ -> "0.000000")) in
      print_endline (String.concat " " parts ^ " 0 1")
