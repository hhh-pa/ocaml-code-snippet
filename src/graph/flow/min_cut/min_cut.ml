let inf = 4_000_000_000_000_000_000L

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let s = Scanf.scanf " %d" (fun x -> x) in
    let t = Scanf.scanf " %d" (fun x -> x) in

    let res = Array.make_matrix n n 0L in
    let w = Array.make_matrix n n 0L in
    for _ = 0 to m - 1 do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      let c = Scanf.scanf " %Ld" (fun x -> x) in
      res.(u).(v) <- Int64.add res.(u).(v) c;
      res.(v).(u) <- Int64.add res.(v).(u) c;
      w.(u).(v) <- Int64.add w.(u).(v) c;
      w.(v).(u) <- Int64.add w.(v).(u) c
    done;

    let flow = ref 0L in
    let par = Array.make n (-1) in
    let continue_flow = ref true in
    while !continue_flow do
      Array.fill par 0 n (-1);
      par.(s) <- s;
      let q = Queue.create () in
      Queue.add s q;
      while not (Queue.is_empty q) && par.(t) = -1 do
        let u = Queue.take q in
        for v = 0 to n - 1 do
          if par.(v) = -1 && res.(u).(v) > 0L then begin
            par.(v) <- u;
            Queue.add v q
          end
        done
      done;
      if par.(t) = -1 then
        continue_flow := false
      else begin
        let b = ref inf in
        let v = ref t in
        while !v <> s do
          let p = par.(!v) in
          if res.(p).(!v) < !b then b := res.(p).(!v);
          v := p
        done;
        let bottleneck = !b in
        v := t;
        while !v <> s do
          let p = par.(!v) in
          res.(p).(!v) <- Int64.sub res.(p).(!v) bottleneck;
          res.(!v).(p) <- Int64.add res.(!v).(p) bottleneck;
          v := p
        done;
        flow := Int64.add !flow bottleneck
      end
    done;

    (* Stoer-Wagner min cut *)
    let alive = Array.make n true in
    let best = ref inf in
    let nn = ref n in
    let added = Array.make n false in
    let ws = Array.make n 0L in
    while !nn > 1 do
      Array.fill added 0 n false;
      Array.fill ws 0 n 0L;
      let pre_last = ref (-1) in
      let last = ref (-1) in
      for k = 0 to !nn - 1 do
        let sel = ref (-1) in
        for v = 0 to n - 1 do
          if alive.(v) && not added.(v) then begin
            if !sel = -1 || ws.(v) > ws.(!sel) then
              sel := v
          end
        done;
        let s_idx = !sel in
        added.(s_idx) <- true;
        for v = 0 to n - 1 do
          if alive.(v) then
            ws.(v) <- Int64.add ws.(v) w.(s_idx).(v)
        done;
        if k = !nn - 2 then pre_last := s_idx;
        if k = !nn - 1 then last := s_idx
      done;
      let coph = ref 0L in
      for v = 0 to n - 1 do
        if alive.(v) && v <> !last then
          coph := Int64.add !coph w.(!last).(v)
      done;
      if !coph < !best then best := !coph;
      for v = 0 to n - 1 do
        w.(!pre_last).(v) <- Int64.add w.(!pre_last).(v) w.(!last).(v);
        w.(v).(!pre_last) <- Int64.add w.(v).(!pre_last) w.(v).(!last)
      done;
      alive.(!last) <- false;
      decr nn
    done;

    Printf.printf "%Ld\n" !flow;
    Printf.printf "%Ld\n" !best
  with End_of_file -> ()
