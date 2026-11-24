(* QR decomposition (NR 2.10, QRdcmp). *)

let fmt v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let sgn a b = if b >= 0.0 then abs_float a else -. abs_float a

let () =
  let toks = String.split_on_char ' ' (String.concat " " (let rec rd acc =
    try rd (input_line stdin :: acc) with End_of_file -> List.rev acc in rd [])) in
  let toks = List.filter (fun s -> s <> "") toks in
  match toks with
  | [] -> ()
  | op :: rest ->
    let nums = List.map float_of_string rest in
    let arr = Array.of_list nums in
    let n = int_of_float arr.(0) in
    let a = Array.init n (fun i -> Array.init n (fun j -> arr.(1 + i * n + j))) in
    let restv = Array.sub arr (1 + n * n) (Array.length arr - 1 - n * n) in
    let b = Array.init n (fun i -> if i < Array.length restv then restv.(i) else 0.0) in
    let u = Array.init n (fun i -> if op = "UPDATE" then restv.(i) else 0.0) in
    let v = Array.init n (fun i -> if op = "UPDATE" then restv.(n + i) else 0.0) in
    let qt = Array.make_matrix n n 0.0 in
    let r = Array.init n (fun i -> Array.copy a.(i)) in
    let c = Array.make n 0.0 in
    let d = Array.make n 0.0 in
    let sing = ref false in
    for k = 0 to n - 2 do
      let scale = ref 0.0 in
      for i = k to n - 1 do scale := max !scale (abs_float r.(i).(k)) done;
      if !scale = 0.0 then begin sing := true; c.(k) <- 0.0; d.(k) <- 0.0 end
      else begin
        for i = k to n - 1 do r.(i).(k) <- r.(i).(k) /. !scale done;
        let sum = ref 0.0 in
        for i = k to n - 1 do sum := !sum +. r.(i).(k) *. r.(i).(k) done;
        let sigma = sgn (sqrt !sum) r.(k).(k) in
        r.(k).(k) <- r.(k).(k) +. sigma;
        c.(k) <- sigma *. r.(k).(k);
        d.(k) <- -. !scale *. sigma;
        for j = k + 1 to n - 1 do
          let s = ref 0.0 in
          for i = k to n - 1 do s := !s +. r.(i).(k) *. r.(i).(j) done;
          let tau = !s /. c.(k) in
          for i = k to n - 1 do r.(i).(j) <- r.(i).(j) -. tau *. r.(i).(k) done
        done
      end
    done;
    d.(n - 1) <- r.(n - 1).(n - 1);
    if d.(n - 1) = 0.0 then sing := true;
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do qt.(i).(j) <- (if i = j then 1.0 else 0.0) done
    done;
    for k = 0 to n - 2 do
      if c.(k) <> 0.0 then
        for j = 0 to n - 1 do
          let sum = ref 0.0 in
          for i = k to n - 1 do sum := !sum +. r.(i).(k) *. qt.(i).(j) done;
          sum := !sum /. c.(k);
          for i = k to n - 1 do qt.(i).(j) <- qt.(i).(j) -. !sum *. r.(i).(k) done
        done
    done;
    for i = 0 to n - 1 do
      r.(i).(i) <- d.(i);
      for j = 0 to i - 1 do r.(i).(j) <- 0.0 done
    done;
    let rotate i aa bb =
      let cc, s =
        if aa = 0.0 then (0.0, if bb >= 0.0 then 1.0 else -1.0)
        else if abs_float aa > abs_float bb then
          let fact = bb /. aa in
          let cc = sgn (1.0 /. sqrt (1.0 +. fact *. fact)) aa in
          (cc, fact *. cc)
        else
          let fact = aa /. bb in
          let s = sgn (1.0 /. sqrt (1.0 +. fact *. fact)) bb in
          (fact *. s, s) in
      for j = i to n - 1 do
        let y = r.(i).(j) and w = r.(i + 1).(j) in
        r.(i).(j) <- cc *. y -. s *. w;
        r.(i + 1).(j) <- s *. y +. cc *. w
      done;
      for j = 0 to n - 1 do
        let y = qt.(i).(j) and w = qt.(i + 1).(j) in
        qt.(i).(j) <- cc *. y -. s *. w;
        qt.(i + 1).(j) <- s *. y +. cc *. w
      done in
    if op = "UPDATE" then begin
      let w = Array.copy u in
      let k = ref (n - 1) in
      while !k >= 0 && w.(!k) = 0.0 do decr k done;
      if !k < 0 then k := 0;
      for i = !k - 1 downto 0 do
        rotate i w.(i) (-. w.(i + 1));
        if w.(i) = 0.0 then w.(i) <- abs_float w.(i + 1)
        else if abs_float w.(i) > abs_float w.(i + 1) then
          let q = w.(i + 1) /. w.(i) in
          w.(i) <- abs_float w.(i) *. sqrt (1.0 +. q *. q)
        else
          let q = w.(i) /. w.(i + 1) in
          w.(i) <- abs_float w.(i + 1) *. sqrt (1.0 +. q *. q)
      done;
      for i = 0 to n - 1 do r.(0).(i) <- r.(0).(i) +. w.(0) *. v.(i) done;
      for i = 0 to !k - 1 do rotate i r.(i).(i) (-. r.(i + 1).(i)) done;
      for i = 0 to n - 1 do if r.(i).(i) = 0.0 then sing := true done
    end;
    if op = "FACTOR" || op = "UPDATE" then begin
      print_endline "QT";
      for i = 0 to n - 1 do
        print_endline (String.concat " " (List.init n (fun j -> fmt qt.(i).(j))))
      done;
      print_endline "R";
      for i = 0 to n - 1 do
        print_endline (String.concat " " (List.init n (fun j -> fmt r.(i).(j))))
      done;
      Printf.printf "SINGULAR %d\n" (if !sing then 1 else 0)
    end else if !sing then print_endline "SINGULAR"
    else begin
      let x = Array.make n 0.0 in
      for i = 0 to n - 1 do
        let sum = ref 0.0 in
        for j = 0 to n - 1 do sum := !sum +. qt.(i).(j) *. b.(j) done;
        x.(i) <- !sum
      done;
      for i = n - 1 downto 0 do
        let sum = ref x.(i) in
        for j = i + 1 to n - 1 do sum := !sum -. r.(i).(j) *. x.(j) done;
        x.(i) <- !sum /. r.(i).(i)
      done;
      print_endline (String.concat " " (List.init n (fun i -> fmt x.(i))))
    end
