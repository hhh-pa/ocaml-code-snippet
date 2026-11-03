let inf = 4_000_000_000_000_000_000L

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let s = Scanf.scanf " %d" (fun x -> x) in
    let adj = Array.make n [] in
    let cmax = ref 0 in
    for _ = 0 to m - 1 do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      let w = Scanf.scanf " %d" (fun x -> x) in
      adj.(u) <- (v, w) :: adj.(u);
      if w > !cmax then cmax := w
    done;
    let maxd = max 1 ((n - 1) * !cmax) in
    let bk = Array.make (maxd + 1) [] in
    bk.(0) <- [s];
    let dist = Array.make n inf in
    dist.(s) <- 0L;
    let done_arr = Array.make n false in
    let idx = ref 0 in
    let pending = ref 1 in
    while !pending > 0 do
      while !idx <= maxd && bk.(!idx) = [] do
        incr idx
      done;
      if !idx <= maxd then begin
        let u = List.hd bk.(!idx) in
        bk.(!idx) <- List.tl bk.(!idx);
        if not done_arr.(u) then begin
          done_arr.(u) <- true;
          let d_u = dist.(u) in
          List.iter (fun (v, w) ->
            let nd = Int64.add d_u (Int64.of_int w) in
            if nd < dist.(v) then begin
              dist.(v) <- nd;
              let nd_int = Int64.to_int nd in
              if nd_int <= maxd then begin
                bk.(nd_int) <- v :: bk.(nd_int);
                incr pending
              end
            end
          ) adj.(u)
        end;
        decr pending
      end else
        pending := 0
    done;
    let half_inf = Int64.div inf 2L in
    for i = 0 to n - 1 do
      if i > 0 then Printf.printf " ";
      if dist.(i) >= half_inf then
        Printf.printf "INF"
      else
        Printf.printf "%Ld" dist.(i)
    done;
    Printf.printf "\n"
  with End_of_file -> ()
