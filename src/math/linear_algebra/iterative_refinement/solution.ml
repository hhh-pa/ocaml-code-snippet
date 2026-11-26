(* Iterative refinement of a linear solve (NR 2.5, mprove).
   *)

let fmt v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let lusolve lu p rhs =
  let n = Array.length lu in
  let y = Array.make n 0.0 in
  let x = Array.make n 0.0 in
  for i = 0 to n - 1 do
    let s = ref rhs.(p.(i)) in
    for j = 0 to i - 1 do s := !s -. lu.(i).(j) *. y.(j) done;
    y.(i) <- !s
  done;
  for i = n - 1 downto 0 do
    let s = ref y.(i) in
    for j = i + 1 to n - 1 do s := !s -. lu.(i).(j) *. x.(j) done;
    x.(i) <- !s /. lu.(i).(i)
  done;
  x

let () =
  let toks = String.split_on_char ' ' (String.concat " " (let rec rd acc =
    try rd (input_line stdin :: acc) with End_of_file -> List.rev acc in rd [])) in
  let toks = List.filter (fun s -> s <> "") toks in
  match toks with
  | [] -> ()
  | _ ->
    let nums = List.map float_of_string toks in
    let arr = Array.of_list nums in
    let n = int_of_float arr.(0) in
    let a = Array.init n (fun i -> Array.init n (fun j -> arr.(1 + i * n + j))) in
    let b = Array.init n (fun i -> arr.(1 + n * n + i)) in
    let lu = Array.init n (fun i -> Array.copy a.(i)) in
    let p = Array.init n (fun i -> i) in
    for k = 0 to n - 1 do
      let piv = ref k in
      for i = k + 1 to n - 1 do
        if abs_float lu.(i).(k) > abs_float lu.(!piv).(k) then piv := i
      done;
      if !piv <> k then begin
        let t = lu.(k) in lu.(k) <- lu.(!piv); lu.(!piv) <- t;
        let ti = p.(k) in p.(k) <- p.(!piv); p.(!piv) <- ti
      end;
      for i = k + 1 to n - 1 do
        lu.(i).(k) <- lu.(i).(k) /. lu.(k).(k);
        for j = k + 1 to n - 1 do lu.(i).(j) <- lu.(i).(j) -. lu.(i).(k) *. lu.(k).(j) done
      done
    done;
    let x = ref (lusolve lu p b) in
    for _round = 0 to 1 do
      let r = Array.make n 0.0 in
      for i = 0 to n - 1 do
        let s = ref (-. b.(i)) in
        let c = ref 0.0 in
        for j = 0 to n - 1 do
          let y = a.(i).(j) *. (!x).(j) -. !c in
          let t = !s +. y in
          c := (t -. !s) -. y;
          s := t
        done;
        r.(i) <- !s
      done;
      let dx = lusolve lu p r in
      for i = 0 to n - 1 do (!x).(i) <- (!x).(i) -. dx.(i) done
    done;
    print_endline (String.concat " " (List.map fmt (Array.to_list !x)))
