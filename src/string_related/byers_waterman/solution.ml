let read_all_tokens () =
  let content =
    let b = Buffer.create 4096 in
    let buf = Bytes.create 4096 in
    let rec loop () =
      let n = input stdin buf 0 4096 in
      if n > 0 then (
        Buffer.add_subbytes b buf 0 n;
        loop ()
      )
    in
    loop ();
    Buffer.contents b
  in
  let len = String.length content in
  let tokens = ref [] in
  let i = ref 0 in
  while !i < len do
    while !i < len && (let c = content.[!i] in c = ' ' || c = '\t' || c = '\r' || c = '\n') do
      incr i
    done;
    if !i < len then (
      let start = !i in
      while !i < len && (let c = content.[!i] in c <> ' ' && c <> '\t' && c <> '\r' && c <> '\n') do
        incr i
      done;
      tokens := String.sub content start (!i - start) :: !tokens
    )
  done;
  Array.of_list (List.rev !tokens)

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 3 then (
    let a = tokens.(0) in
    let b = tokens.(1) in
    let d = int_of_string tokens.(2) in
    let n = String.length a in
    let m = String.length b in

    let f = Array.make_matrix (n + 1) (m + 1) 0 in
    for i = 0 to n do f.(i).(0) <- i done;
    for j = 0 to m do f.(0).(j) <- j done;
    for i = 1 to n do
      let ai = a.[i - 1] in
      for j = 1 to m do
        let c = if ai = b.[j - 1] then 0 else 1 in
        f.(i).(j) <- min (min (f.(i - 1).(j) + 1) (f.(i).(j - 1) + 1)) (f.(i - 1).(j - 1) + c)
      done
    done;

    let r = Array.make_matrix (n + 1) (m + 1) 0 in
    for i = 0 to n do r.(i).(m) <- n - i done;
    for j = 0 to m do r.(n).(j) <- m - j done;
    for i = n - 1 downto 0 do
      let ai = a.[i] in
      for j = m - 1 downto 0 do
        let c = if ai = b.[j] then 0 else 1 in
        r.(i).(j) <- min (min (r.(i + 1).(j) + 1) (r.(i).(j + 1) + 1)) (r.(i + 1).(j + 1) + c)
      done
    done;

    let opt = f.(n).(m) in
    let max_cost = opt + d in
    let out = ref [] in
    let ca = Buffer.create (n + m) in
    let cb = Buffer.create (n + m) in

    let rec dfs i j g =
      if i = n && j = m then (
        out := (Buffer.contents ca, Buffer.contents cb) :: !out
      ) else (
        (* 0: diag *)
        if i < n && j < m then (
          let g2 = g + (if a.[i] = b.[j] then 0 else 1) in
          if g2 + r.(i + 1).(j + 1) <= max_cost then (
            Buffer.add_char ca a.[i];
            Buffer.add_char cb b.[j];
            dfs (i + 1) (j + 1) g2;
            Buffer.truncate ca (Buffer.length ca - 1);
            Buffer.truncate cb (Buffer.length cb - 1)
          )
        );
        (* 1: down (gap in b) *)
        if i < n then (
          let g2 = g + 1 in
          if g2 + r.(i + 1).(j) <= max_cost then (
            Buffer.add_char ca a.[i];
            Buffer.add_char cb '-';
            dfs (i + 1) j g2;
            Buffer.truncate ca (Buffer.length ca - 1);
            Buffer.truncate cb (Buffer.length cb - 1)
          )
        );
        (* 2: right (gap in a) *)
        if j < m then (
          let g2 = g + 1 in
          if g2 + r.(i).(j + 1) <= max_cost then (
            Buffer.add_char ca '-';
            Buffer.add_char cb b.[j];
            dfs i (j + 1) g2;
            Buffer.truncate ca (Buffer.length ca - 1);
            Buffer.truncate cb (Buffer.length cb - 1)
          )
        )
      )
    in
    dfs 0 0 0;
    let res = List.sort compare !out in
    Printf.printf "%d\n" (List.length res);
    List.iter (fun (sa, sb) ->
      Printf.printf "%s\n%s\n" sa sb
    ) res
  )
