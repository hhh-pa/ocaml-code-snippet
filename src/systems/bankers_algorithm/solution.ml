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
  if Array.length tokens >= 2 then (
    let n = int_of_string tokens.(0) in
    let r = int_of_string tokens.(1) in
    let idx = ref 2 in
    let avail = Array.init r (fun _ ->
      let v = int_of_string tokens.(!idx) in
      incr idx;
      v
    ) in
    let alloc = Array.init n (fun _ ->
      Array.init r (fun _ ->
        let v = int_of_string tokens.(!idx) in
        incr idx;
        v
      )
    ) in
    let mx = Array.init n (fun _ ->
      Array.init r (fun _ ->
        let v = int_of_string tokens.(!idx) in
        incr idx;
        v
      )
    ) in

    let work = Array.copy avail in
    let done_arr = Array.make n false in
    let seq = ref [] in
    let cont = ref true in
    while !cont do
      let pick = ref (-1) in
      let i = ref 0 in
      while !i < n && !pick = -1 do
        if not done_arr.(!i) then (
          let ok = ref true in
          for j = 0 to r - 1 do
            if mx.(!i).(j) - alloc.(!i).(j) > work.(j) then
              ok := false
          done;
          if !ok then pick := !i
        );
        incr i
      done;
      if !pick = -1 then cont := false
      else (
        let p = !pick in
        done_arr.(p) <- true;
        seq := p :: !seq;
        for j = 0 to r - 1 do
          work.(j) <- work.(j) + alloc.(p).(j)
        done
      )
    done;

    let res = List.rev !seq in
    if List.length res = n then (
      let strs = List.map string_of_int res in
      Printf.printf "SAFE %s\n" (String.concat " " strs)
    ) else (
      Printf.printf "UNSAFE %d\n" (List.length res)
    )
  )
