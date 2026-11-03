let inf = 1_000_000_000_000_000_000L

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let cost = Array.make_matrix (n + 1) (m + 1) 0L in
    for i = 1 to n do
      for j = 1 to m do
        cost.(i).(j) <- Scanf.scanf " %Ld" (fun x -> x)
      done
    done;
    let u = Array.make (n + 1) 0L in
    let v = Array.make (m + 1) 0L in
    let p = Array.make (m + 1) 0 in
    let way = Array.make (m + 1) 0 in

    for i = 1 to n do
      p.(0) <- i;
      let j0 = ref 0 in
      let minv = Array.make (m + 1) inf in
      let used = Array.make (m + 1) false in
      let step_done = ref false in
      while not !step_done do
        used.(!j0) <- true;
        let i0 = p.(!j0) in
        let delta = ref inf in
        let j1 = ref 0 in
        for j = 1 to m do
          if not used.(j) then begin
            let cur = Int64.sub (Int64.sub cost.(i0).(j) u.(i0)) v.(j) in
            if cur < minv.(j) then begin
              minv.(j) <- cur;
              way.(j) <- !j0
            end;
            if minv.(j) < !delta then begin
              delta := minv.(j);
              j1 := j
            end
          end
        done;
        for j = 0 to m do
          if used.(j) then begin
            u.(p.(j)) <- Int64.add u.(p.(j)) !delta;
            v.(j) <- Int64.sub v.(j) !delta
          end else begin
            minv.(j) <- Int64.sub minv.(j) !delta
          end
        done;
        j0 := !j1;
        if p.(!j0) = 0 then step_done := true
      done;

      let trace_done = ref false in
      while not !trace_done do
        let j1 = way.(!j0) in
        p.(!j0) <- p.(j1);
        j0 := j1;
        if !j0 = 0 then trace_done := true
      done
    done;

    let min_cost = Int64.neg v.(0) in
    Printf.printf "%Ld\n" min_cost;
    let match_arr = Array.make (n + 1) 0 in
    for j = 1 to m do
      if p.(j) <= n then
        match_arr.(p.(j)) <- j
    done;
    for i = 1 to n do
      Printf.printf "%d %d\n" i match_arr.(i)
    done
  with End_of_file -> ()
