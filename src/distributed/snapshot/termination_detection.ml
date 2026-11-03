let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let e_count = Scanf.scanf " %d" (fun x -> x) in
    let engaged = Array.make n 0 in
    let parent = Array.make n (-1) in
    let idlepend = Array.make n 0 in
    let c = Array.make n 0L in
    let d = Array.make n 0L in
    engaged.(0) <- 1;
    let signals = ref 0L in
    let ignored = ref 0L in
    let term_at = ref (-1) in
    let ev = Array.make e_count "" in
    let a = Array.make e_count 0 in
    let b = Array.make e_count 0 in
    for k = 0 to e_count - 1 do
      let op = Scanf.scanf " %s" (fun x -> x) in
      ev.(k) <- op;
      if op = "SEND" then begin
        a.(k) <- Scanf.scanf " %d" (fun x -> x);
        b.(k) <- Scanf.scanf " %d" (fun x -> x)
      end else
        a.(k) <- Scanf.scanf " %d" (fun x -> x)
    done;
    let continue_loop = ref true in
    let k = ref 0 in
    while !k < e_count && !continue_loop do
      let e = !k + 1 in
      if !term_at >= 0 then
        continue_loop := false
      else if ev.(!k) = "SEND" then begin
        let u = a.(!k) in
        let v = b.(!k) in
        if engaged.(u) = 0 then
          ignored := Int64.add !ignored 1L
        else begin
          d.(u) <- Int64.add d.(u) 1L;
          if engaged.(v) = 0 then begin
            engaged.(v) <- 1;
            parent.(v) <- u;
            c.(u) <- Int64.add c.(u) 1L
          end else begin
            d.(u) <- Int64.sub d.(u) 1L;
            signals := Int64.add !signals 1L
          end
        end
      end else begin
        let i = a.(!k) in
        if engaged.(i) <> 0 then begin
          engaged.(i) <- 0;
          let x = ref i in
          let inner_loop = ref true in
          while !inner_loop do
            if d.(!x) > 0L then begin
              idlepend.(!x) <- 1;
              inner_loop := false
            end else begin
              idlepend.(!x) <- 0;
              if !x = 0 then begin
                if c.(0) = 0L then term_at := e;
                inner_loop := false
              end else begin
                let p = parent.(!x) in
                c.(p) <- Int64.sub c.(p) 1L;
                d.(p) <- Int64.sub d.(p) 1L;
                signals := Int64.add !signals 1L;
                if engaged.(p) <> 0 then
                  inner_loop := false
                else
                  x := p
              end
            end
          done
        end
      end;
      incr k
    done;
    Printf.printf "TERMINATED: ";
    if !term_at >= 0 then
      Printf.printf "%d\n" !term_at
    else
      Printf.printf "NEVER\n";
    Printf.printf "SIGNALS: %Ld\n" !signals;
    Printf.printf "IGNORED: %Ld\n" !ignored
  with End_of_file -> ()
