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

let inf_ll = 4000000000000000000L
let half_inf = 2000000000000000000L

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 2 then (
    let n = int_of_string tokens.(0) in
    let e = int_of_string tokens.(1) in
    let cost = Array.make_matrix n n inf_ll in
    let idx = ref 2 in
    for _ = 0 to e - 1 do
      let u = int_of_string tokens.(!idx) in
      let v = int_of_string tokens.(!idx + 1) in
      let c = Int64.of_string tokens.(!idx + 2) in
      idx := !idx + 3;
      cost.(u).(v) <- c;
      cost.(v).(u) <- c
    done;

    for s = 0 to n - 1 do
      let dist = Array.make n inf_ll in
      let parent = Array.make n (-1) in
      let done_arr = Array.make n false in
      dist.(s) <- 0L;

      for _ = 0 to n - 1 do
        let u = ref (-1) in
        for i = 0 to n - 1 do
          if not done_arr.(i) && (!u = -1 || dist.(i) < dist.(!u)) then
            u := i
        done;
        let p = !u in
        if p = -1 || dist.(p) = inf_ll then ()
        else (
          done_arr.(p) <- true;
          let dist_p = dist.(p) in
          for v = 0 to n - 1 do
            let c = cost.(p).(v) in
            if c < half_inf then (
              let cand = Int64.add dist_p c in
              if cand < dist.(v) then (
                dist.(v) <- cand;
                parent.(v) <- p
              )
            )
          done
        )
      done;

      let line_tokens = ref [] in
      for d = 0 to n - 1 do
        if d = s then
          line_tokens := "0:-" :: !line_tokens
        else if dist.(d) >= half_inf then
          line_tokens := "INF:-" :: !line_tokens
        else (
          let x = ref d in
          while parent.(!x) <> s do
            x := parent.(!x)
          done;
          line_tokens := Printf.sprintf "%Ld:%d" dist.(d) !x :: !line_tokens
        )
      done;
      print_endline (String.concat " " (List.rev !line_tokens))
    done
  )
