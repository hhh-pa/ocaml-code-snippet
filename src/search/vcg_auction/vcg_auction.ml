let solve_case () =
  let n = Scanf.scanf " %d" (fun x -> x) in
  let _m = Scanf.scanf " %d" (fun x -> x) in
  let b = Scanf.scanf " %d" (fun x -> x) in
  let bb = Array.make b 0 in
  let bmask = Array.make b 0 in
  let bv = Array.make b 0 in
  for i = 0 to b - 1 do
    let bi = Scanf.scanf " %d" (fun x -> x) in
    let k = Scanf.scanf " %d" (fun x -> x) in
    let m = ref 0 in
    for _ = 0 to k - 1 do
      let x = Scanf.scanf " %d" (fun x -> x) in
      m := !m lor (1 lsl x)
    done;
    let v = Scanf.scanf " %d" (fun x -> x) in
    bb.(i) <- bi;
    bmask.(i) <- !m;
    bv.(i) <- v
  done;

  let welfare skip_bidder =
    let best = ref 0 in
    let bestm = ref 0 in
    let limit = 1 lsl b in
    for m = 0 to limit - 1 do
      let used = ref 0 in
      let tot = ref 0 in
      let ok = ref true in
      let i = ref 0 in
      while !i < b && !ok do
        if (m lsr !i) land 1 = 1 then begin
          if skip_bidder >= 0 && bb.(!i) = skip_bidder then ok := false
          else if !used land bmask.(!i) <> 0 then ok := false
          else begin
            used := !used lor bmask.(!i);
            tot := !tot + bv.(!i)
          end
        end;
        incr i
      done;
      if !ok && !tot > !best then begin
        best := !tot;
        bestm := m
      end
    done;
    let won = ref [] in
    for i = 0 to b - 1 do
      if (!bestm lsr i) land 1 = 1 then won := i :: !won
    done;
    (!best, List.rev !won)
  in

  let (w_val, won) = welfare (-1) in
  Printf.printf "VCG %d\n" w_val;
  Printf.printf "PAY";
  for i = 0 to n - 1 do
    let mine = ref 0 in
    List.iter (fun w ->
      if bb.(w) = i then mine := !mine + bv.(w)
    ) won;
    let (wo, _) = welfare i in
    Printf.printf " %d" (wo - (w_val - !mine))
  done;
  print_newline ()

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
