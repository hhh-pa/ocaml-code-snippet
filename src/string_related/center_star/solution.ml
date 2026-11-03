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

let edist a b =
  let n = String.length a in
  let m = String.length b in
  let dp = Array.make_matrix (n + 1) (m + 1) 0 in
  for i = 0 to n do dp.(i).(0) <- i done;
  for j = 0 to m do dp.(0).(j) <- j done;
  for i = 1 to n do
    let ai = a.[i - 1] in
    for j = 1 to m do
      let c = if ai = b.[j - 1] then 0 else 1 in
      dp.(i).(j) <- min (min (dp.(i - 1).(j) + 1) (dp.(i).(j - 1) + 1)) (dp.(i - 1).(j - 1) + c)
    done
  done;
  dp.(n).(m)

let align2 c s =
  let n = String.length c in
  let m = String.length s in
  let dp = Array.make_matrix (n + 1) (m + 1) 0 in
  for i = 0 to n do dp.(i).(0) <- i done;
  for j = 0 to m do dp.(0).(j) <- j done;
  for i = 1 to n do
    let ci = c.[i - 1] in
    for j = 1 to m do
      let cc = if ci = s.[j - 1] then 0 else 1 in
      dp.(i).(j) <- min (min (dp.(i - 1).(j) + 1) (dp.(i).(j - 1) + 1)) (dp.(i - 1).(j - 1) + cc)
    done
  done;
  let rc = Buffer.create (n + m) in
  let rs = Buffer.create (n + m) in
  let i = ref n in
  let j = ref m in
  while !i > 0 || !j > 0 do
    let cur = dp.(!i).(!j) in
    if !i > 0 && !j > 0 && cur = dp.(!i - 1).(!j - 1) + (if c.[!i - 1] = s.[!j - 1] then 0 else 1) then (
      Buffer.add_char rc c.[!i - 1];
      Buffer.add_char rs s.[!j - 1];
      decr i;
      decr j
    ) else if !i > 0 && cur = dp.(!i - 1).(!j) + 1 then (
      Buffer.add_char rc c.[!i - 1];
      Buffer.add_char rs '-';
      decr i
    ) else (
      Buffer.add_char rc '-';
      Buffer.add_char rs s.[!j - 1];
      decr j
    )
  done;
  let rev_str b =
    let s = Buffer.contents b in
    let len = String.length s in
    let res = Bytes.create len in
    for k = 0 to len - 1 do
      Bytes.set res k s.[len - 1 - k]
    done;
    Bytes.to_string res
  in
  (rev_str rc, rev_str rs)

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 1 then (
    let n = int_of_string tokens.(0) in
    let strs = Array.init n (fun i -> tokens.(i + 1)) in
    let best = ref 0 in
    let besttot = ref (-1) in
    for c = 0 to n - 1 do
      let tot = ref 0 in
      for i = 0 to n - 1 do
        tot := !tot + edist strs.(c) strs.(i)
      done;
      if !besttot < 0 || !tot < !besttot then (
        besttot := !tot;
        best := c
      )
    done;
    let b = !best in
    let rows = ref [strs.(b)] in
    let rowpos = Array.make n (-1) in
    rowpos.(b) <- 0;
    for idx = 0 to n - 1 do
      if idx <> b then (
        let (n_str, s_str) = align2 strs.(b) strs.(idx) in
        let cur_rows = Array.of_list !rows in
        let num_rows = Array.length cur_rows in
        let r_str = cur_rows.(0) in
        let nr = Array.init (num_rows + 1) (fun _ -> Buffer.create 32) in
        let i = ref 0 in
        let j = ref 0 in
        let len_r = String.length r_str in
        let len_n = String.length n_str in
        while !i < len_r || !j < len_n do
          if !j < len_n && n_str.[!j] = '-' then (
            for r = 0 to num_rows - 1 do
              Buffer.add_char nr.(r) '-'
            done;
            Buffer.add_char nr.(num_rows) s_str.[!j];
            incr j
          ) else if !i < len_r && r_str.[!i] = '-' then (
            for r = 0 to num_rows - 1 do
              Buffer.add_char nr.(r) cur_rows.(r).[!i]
            done;
            Buffer.add_char nr.(num_rows) '-';
            incr i
          ) else (
            for r = 0 to num_rows - 1 do
              Buffer.add_char nr.(r) cur_rows.(r).[!i]
            done;
            Buffer.add_char nr.(num_rows) s_str.[!j];
            incr i;
            incr j
          )
        done;
        let new_rows = Array.init (num_rows + 1) (fun r -> Buffer.contents nr.(r)) in
        rows := Array.to_list new_rows;
        rowpos.(idx) <- num_rows
      )
    done;
    let final_rows = Array.of_list !rows in
    let num_r = Array.length final_rows in
    let len_l = String.length final_rows.(0) in
    let sp = ref 0 in
    for c = 0 to len_l - 1 do
      for a = 0 to num_r - 1 do
        for b = a + 1 to num_r - 1 do
          if final_rows.(a).[c] <> final_rows.(b).[c] then incr sp
        done
      done
    done;
    Printf.printf "%d\n" !sp;
    for i = 0 to n - 1 do
      Printf.printf "%s\n" final_rows.(rowpos.(i))
    done
  )
