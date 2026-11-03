let inf = 4_000_000_000_000_000_000L

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let s = Scanf.scanf " %d" (fun x -> x) in
    let t = Scanf.scanf " %d" (fun x -> x) in
    let res = Array.make_matrix n n 0L in
    for _ = 0 to m - 1 do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      let c = Scanf.scanf " %Ld" (fun x -> x) in
      res.(u).(v) <- Int64.add res.(u).(v) c
    done;

    let flow = ref 0L in
    let continue_loop = ref true in
    let par = Array.make n (-1) in
    while !continue_loop do
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
        continue_loop := false
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
    Printf.printf "%Ld\n" !flow
  with End_of_file -> ()
