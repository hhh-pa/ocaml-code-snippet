(* Cubic spline interpolation (NR 3.3). Port of the C++ reference. *)

let fmt v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

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
    let xv = Array.init n (fun i -> arr.(1 + i)) in
    let yv = Array.init n (fun i -> arr.(1 + n + i)) in
    let yp1 = arr.(1 + 2 * n) in
    let ypn = arr.(1 + 2 * n + 1) in
    let y2 = Array.make n 0.0 in
    let u = Array.make (n - 1) 0.0 in
    if yp1 > 0.99e99 then begin y2.(0) <- 0.0; u.(0) <- 0.0 end
    else begin
      y2.(0) <- -0.5;
      u.(0) <- (3.0 /. (xv.(1) -. xv.(0))) *. ((yv.(1) -. yv.(0)) /. (xv.(1) -. xv.(0)) -. yp1)
    end;
    for i = 1 to n - 2 do
      let sg = (xv.(i) -. xv.(i - 1)) /. (xv.(i + 1) -. xv.(i - 1)) in
      let p = sg *. y2.(i - 1) +. 2.0 in
      y2.(i) <- (sg -. 1.0) /. p;
      let ui = (yv.(i + 1) -. yv.(i)) /. (xv.(i + 1) -. xv.(i))
        -. (yv.(i) -. yv.(i - 1)) /. (xv.(i) -. xv.(i - 1)) in
      u.(i) <- (6.0 *. ui /. (xv.(i + 1) -. xv.(i - 1)) -. sg *. u.(i - 1)) /. p
    done;
    let qn, un =
      if ypn > 0.99e99 then (0.0, 0.0)
      else (0.5, (3.0 /. (xv.(n - 1) -. xv.(n - 2))) *.
        (ypn -. (yv.(n - 1) -. yv.(n - 2)) /. (xv.(n - 1) -. xv.(n - 2)))) in
    y2.(n - 1) <- (un -. qn *. u.(n - 2)) /. (qn *. y2.(n - 2) +. 1.0);
    for k = n - 2 downto 0 do y2.(k) <- y2.(k) *. y2.(k + 1) +. u.(k) done;
    if op = "SECOND" then
      print_endline (String.concat " " (List.init n (fun i -> fmt y2.(i))))
    else begin
      let q = int_of_float arr.(1 + 2 * n + 2) in
      for qq = 0 to q - 1 do
        let x = arr.(1 + 2 * n + 3 + qq) in
        let klo = ref 0 and khi = ref (n - 1) in
        while !khi - !klo > 1 do
          let k = (!khi + !klo) lsr 1 in
          if xv.(k) > x then khi := k else klo := k
        done;
        let jl = min !klo (n - 2) in
        let klo2, khi2 = jl, jl + 1 in
        let h = xv.(khi2) -. xv.(klo2) in
        let a = (xv.(khi2) -. x) /. h in
        let b = (x -. xv.(klo2)) /. h in
        let y = a *. yv.(klo2) +. b *. yv.(khi2)
          +. ((a *. a *. a -. a) *. y2.(klo2) +. (b *. b *. b -. b) *. y2.(khi2)) *. (h *. h) /. 6.0 in
        print_endline (fmt y)
      done
    end
