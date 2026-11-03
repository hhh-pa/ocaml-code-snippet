let () =
  try
    let d_in = Scanf.scanf " %d" (fun x -> x) in
    let q = Scanf.scanf " %d" (fun x -> x) in
    let d = max 2 d_in in
    let a = Array.make (max 1 q) 0L in
    let sz = ref 0 in

    let swim idx =
      let k = ref idx in
      let cont = ref true in
      while !k > 0 && !cont do
        let p = (!k - 1) / d in
        if a.(!k) >= a.(p) then
          cont := false
        else begin
          let t = a.(!k) in
          a.(!k) <- a.(p);
          a.(p) <- t;
          k := p
        end
      done
    in

    let sink idx =
      let k = ref idx in
      let cont = ref true in
      while !cont do
        let m = ref !k in
        let base = d * !k in
        for j = 1 to d do
          let c = base + j in
          if c < !sz && a.(c) < a.(!m) then
            m := c
        done;
        if !m = !k then
          cont := false
        else begin
          let t = a.(!k) in
          a.(!k) <- a.(!m);
          a.(!m) <- t;
          k := !m
        end
      done
    in

    let insert x =
      let idx = !sz in
      a.(idx) <- x;
      incr sz;
      swim idx
    in

    let extract_min () =
      if !sz = 0 then None
      else begin
        let res = a.(0) in
        decr sz;
        if !sz > 0 then begin
          a.(0) <- a.(!sz);
          sink 0
        end;
        Some res
      end
    in

    for _ = 1 to q do
      let op = Scanf.scanf " %d" (fun x -> x) in
      if op = 1 then
        let x = Scanf.scanf " %Ld" (fun x -> x) in
        insert x
      else if op = 2 then
        match extract_min () with
        | Some o -> Printf.printf "%Ld\n" o
        | None -> Printf.printf "EMPTY\n"
      else if op = 3 then
        if !sz = 0 then Printf.printf "EMPTY\n"
        else Printf.printf "%Ld\n" a.(0)
    done
  with End_of_file -> ()
