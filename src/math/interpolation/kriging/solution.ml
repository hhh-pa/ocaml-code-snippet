(* Kriging interpolation (NR 3.7.6). Port of the C++ reference. *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

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
  let next () =
    let v = toks.(!pos) in
    incr pos; v
  in
  let beta = float_of_string (next ()) in
  let nug = float_of_string (next ()) in
  let n = int_of_string (next ()) in
  let dim = int_of_string (next ()) in
  let pts = Array.make_matrix n dim 0.0 in
  for i = 0 to n - 1 do
    for j = 0 to dim - 1 do
      pts.(i).(j) <- float_of_string (next ())
    done
  done;
  let vals = Array.make n 0.0 in
  for i = 0 to n - 1 do
    vals.(i) <- float_of_string (next ())
  done;
  let nugsq = nug *. nug in
  let rdist a b =
    let d = ref 0.0 in
    for i = 0 to dim - 1 do
      d := !d +. (a.(i) -. b.(i)) *. (a.(i) -. b.(i))
    done;
    sqrt !d
  in
  let num = ref 0.0 and denom = ref 0.0 in
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      let rb = ref 0.0 in
      for k = 0 to dim - 1 do
        rb := !rb +. (pts.(i).(k) -. pts.(j).(k)) *. (pts.(i).(k) -. pts.(j).(k))
      done;
      let rb = !rb ** (0.5 *. beta) in
      num :=
        !num +. rb *. (0.5 *. (vals.(i) -. vals.(j)) *. (vals.(i) -. vals.(j)) -. nugsq);
      denom := !denom +. rb *. rb
    done
  done;
  let alph = !num /. !denom in
  let vgram r = nugsq +. alph *. r ** beta in
  let m = n + 1 in
  let v = Array.make_matrix m m 0.0 in
  let y = Array.make m 0.0 in
  for i = 0 to n - 1 do
    y.(i) <- vals.(i);
    for j = i to n - 1 do
      let g = vgram (rdist pts.(i) pts.(j)) in
      v.(i).(j) <- g;
      v.(j).(i) <- g
    done;
    v.(i).(n) <- 1.0;
    v.(n).(i) <- 1.0
  done;
  let p = Array.init m (fun i -> i) in
  for k = 0 to m - 1 do
    let piv = ref k in
    for i = k + 1 to m - 1 do
      if abs_float v.(i).(k) > abs_float v.(!piv).(k) then piv := i
    done;
    if !piv <> k then (
      let t = v.(k) in
      v.(k) <- v.(!piv);
      v.(!piv) <- t;
      let tp = p.(k) in
      p.(k) <- p.(!piv);
      p.(!piv) <- tp);
    for i = k + 1 to m - 1 do
      v.(i).(k) <- v.(i).(k) /. v.(k).(k);
      for j = k + 1 to m - 1 do
        v.(i).(j) <- v.(i).(j) -. v.(i).(k) *. v.(k).(j)
      done
    done
  done;
  let lusolve rhs =
    let yy = Array.make m 0.0 and x = Array.make m 0.0 in
    for i = 0 to m - 1 do
      let s = ref rhs.(p.(i)) in
      for j = 0 to i - 1 do
        s := !s -. v.(i).(j) *. yy.(j)
      done;
      yy.(i) <- !s
    done;
    for i = m - 1 downto 0 do
      let s = ref yy.(i) in
      for j = i + 1 to m - 1 do
        s := !s -. v.(i).(j) *. x.(j)
      done;
      x.(i) <- !s /. v.(i).(i)
    done;
    x
  in
  let yvi = lusolve y in
  let q = int_of_string (next ()) in
  for _qq = 0 to q - 1 do
    let pt = Array.make dim 0.0 in
    for j = 0 to dim - 1 do
      pt.(j) <- float_of_string (next ())
    done;
    let vstar = Array.make m 0.0 in
    for i = 0 to n - 1 do
      vstar.(i) <- vgram (rdist pt pts.(i))
    done;
    vstar.(n) <- 1.0;
    let lastval = ref 0.0 in
    for i = 0 to n do
      lastval := !lastval +. yvi.(i) *. vstar.(i)
    done;
    let dstar = lusolve vstar in
    let lasterr = ref 0.0 in
    for i = 0 to n do
      lasterr := !lasterr +. dstar.(i) *. vstar.(i)
    done;
    print_string (fmt6 !lastval);
    print_string " ";
    print_string (fmt6 (sqrt (max 0.0 !lasterr)));
    print_newline ()
  done
