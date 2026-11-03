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

let rec remove_first x = function
  | [] -> []
  | h :: t -> if h = x then t else h :: remove_first x t

let rec remove_last = function
  | [] -> []
  | [_] -> []
  | h :: t -> h :: remove_last t

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 4 then (
    let s = int_of_string tokens.(0) in
    let e = int_of_string tokens.(1) in
    let b = int_of_string tokens.(2) in
    let n = int_of_string tokens.(3) in
    let sets = Array.make s [] in
    let hits = ref 0 in
    let misses = ref 0 in
    let evict = ref 0 in

    for k = 0 to n - 1 do
      let addr = int_of_string tokens.(4 + k) in
      let block = addr / b in
      let set_idx = block mod s in
      let tag = block / s in
      let lines = sets.(set_idx) in
      if List.mem tag lines then (
        incr hits;
        sets.(set_idx) <- tag :: (remove_first tag lines)
      ) else (
        incr misses;
        let cur =
          if List.length lines >= e then (
            incr evict;
            remove_last lines
          ) else lines
        in
        sets.(set_idx) <- tag :: cur
      )
    done;

    Printf.printf "%d %d %d\n" !hits !misses !evict
  )
