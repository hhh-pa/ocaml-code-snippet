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

let overlap a b =
  let la = String.length a in
  let lb = String.length b in
  let max_k = min la lb in
  let res = ref 0 in
  let k = ref max_k in
  while !k > 0 && !res = 0 do
    if String.sub a (la - !k) !k = String.sub b 0 !k then
      res := !k;
    decr k
  done;
  !res

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 1 then (
    let n = int_of_string tokens.(0) in
    let strs = ref (Array.to_list (Array.init n (fun i -> tokens.(i + 1)))) in
    while List.length !strs > 1 do
      let arr = Array.of_list !strs in
      let sz = Array.length arr in
      let best_i = ref (-1) in
      let best_j = ref (-1) in
      let best_ov = ref (-1) in
      let best_merged = ref "" in
      for i = 0 to sz - 1 do
        for j = 0 to sz - 1 do
          if i <> j then (
            let ov = overlap arr.(i) arr.(j) in
            if ov > !best_ov then (
              best_ov := ov;
              best_i := i;
              best_j := j;
              best_merged := arr.(i) ^ (String.sub arr.(j) ov (String.length arr.(j) - ov))
            )
          )
        done
      done;
      let nxt = ref [] in
      for k = 0 to sz - 1 do
        if k <> !best_i && k <> !best_j then
          nxt := arr.(k) :: !nxt
      done;
      nxt := !best_merged :: !nxt;
      strs := List.rev !nxt
    done;
    if !strs <> [] then
      print_endline (List.hd !strs)
  )
