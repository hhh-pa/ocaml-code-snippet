(* Toeplitz solve in O(n^2) (NR 2.8.2, toeplz). *)

let fmt v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

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
    let r = Array.init (2 * n - 1) (fun i -> arr.(1 + i)) in
    let y = Array.init n (fun i -> arr.(1 + 2 * n - 1 + i)) in
    let x = Array.make n 0.0 in
    let n1 = n - 1 in
    x.(0) <- y.(0) /. r.(n1);
    if n1 <> 0 then begin
      let g = Array.make n1 0.0 in
      let h = Array.make n1 0.0 in
      g.(0) <- r.(n1 - 1) /. r.(n1);
      h.(0) <- r.(n1 + 1) /. r.(n1);
      let m = ref 0 in
      let running = ref true in
      while !running && !m < n do
        let m1 = !m + 1 in
        let sxn = ref (-. y.(m1)) in
        let sd = ref (-. r.(n1)) in
        for j = 0 to !m do
          sxn := !sxn +. r.(n1 + m1 - j) *. x.(j);
          sd := !sd +. r.(n1 + m1 - j) *. g.(!m - j)
        done;
        x.(m1) <- !sxn /. !sd;
        for j = 0 to !m do x.(j) <- x.(j) -. x.(m1) *. g.(!m - j) done;
        if m1 = n1 then running := false
        else begin
          let sgn = ref (-. r.(n1 - m1 - 1)) in
          let shn = ref (-. r.(n1 + m1 + 1)) in
          let sgd = ref (-. r.(n1)) in
          for j = 0 to !m do
            sgn := !sgn +. r.(n1 + j - m1) *. g.(j);
            shn := !shn +. r.(n1 + m1 - j) *. h.(j);
            sgd := !sgd +. r.(n1 + j - m1) *. h.(!m - j)
          done;
          g.(m1) <- !sgn /. !sgd;
          h.(m1) <- !shn /. !sd;
          let k = ref !m in
          let m2 = (!m + 2) lsr 1 in
          let pp = g.(m1) and qq = h.(m1) in
          for j = 0 to m2 - 1 do
            let pt1 = g.(j) and pt2 = g.(!k) and qt1 = h.(j) and qt2 = h.(!k) in
            g.(j) <- pt1 -. pp *. qt2;
            g.(!k) <- pt2 -. pp *. qt1;
            h.(j) <- qt1 -. qq *. pt2;
            h.(!k) <- qt2 -. qq *. pt1;
            decr k
          done;
          incr m
        end
      done
    end;
    print_endline (String.concat " " (List.init n (fun i -> fmt x.(i))))
