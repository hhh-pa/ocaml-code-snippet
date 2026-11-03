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

type index_min_pq = {
  n: int;
  mutable sz: int;
  pq: int array;
  qp: int array;
  keys: int64 array;
}

let make_pq n = {
  n;
  sz = 0;
  pq = Array.make (n + 1) 0;
  qp = Array.make n (-1);
  keys = Array.make n 0L;
}

let less pq i j =
  if pq.keys.(i) <> pq.keys.(j) then
    pq.keys.(i) < pq.keys.(j)
  else
    i < j

let exch pq a b =
  let t = pq.pq.(a) in
  pq.pq.(a) <- pq.pq.(b);
  pq.pq.(b) <- t;
  pq.qp.(pq.pq.(a)) <- a;
  pq.qp.(pq.pq.(b)) <- b

let swim pq idx =
  let k = ref idx in
  while !k > 1 && less pq pq.pq.(!k) pq.pq.(!k / 2) do
    exch pq !k (!k / 2);
    k := !k / 2
  done

let sink pq idx =
  let k = ref idx in
  let cont = ref true in
  while 2 * !k <= pq.sz && !cont do
    let j = ref (2 * !k) in
    if !j < pq.sz && less pq pq.pq.(!j + 1) pq.pq.(!j) then
      incr j;
    if not (less pq pq.pq.(!j) pq.pq.(!k)) then
      cont := false
    else begin
      exch pq !k !j;
      k := !j
    end
  done

let change pq i key =
  pq.keys.(i) <- key;
  swim pq pq.qp.(i);
  sink pq pq.qp.(i)

let insert pq i key =
  if pq.qp.(i) <> -1 then
    change pq i key
  else begin
    pq.sz <- pq.sz + 1;
    pq.qp.(i) <- pq.sz;
    pq.pq.(pq.sz) <- i;
    pq.keys.(i) <- key;
    swim pq pq.sz
  end

let has pq i =
  i >= 0 && i < pq.n && pq.qp.(i) <> -1

let min_index pq = pq.pq.(1)

let del_min pq =
  let m = pq.pq.(1) in
  exch pq 1 pq.sz;
  pq.sz <- pq.sz - 1;
  sink pq 1;
  pq.qp.(m) <- -1;
  m

let erase pq i =
  let k = pq.qp.(i) in
  exch pq k pq.sz;
  pq.sz <- pq.sz - 1;
  swim pq k;
  sink pq k;
  pq.qp.(i) <- -1

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 2 then begin
    let n = int_of_string tokens.(0) in
    let q = int_of_string tokens.(1) in
    let pq = make_pq n in
    let idx = ref 2 in
    for _ = 1 to q do
      if !idx < Array.length tokens then begin
        let op = int_of_string tokens.(!idx) in
        incr idx;
        match op with
        | 1 ->
          let i = int_of_string tokens.(!idx) in
          let k = Int64.of_string tokens.(!idx + 1) in
          idx := !idx + 2;
          if i >= 0 && i < n then insert pq i k
        | 2 ->
          if pq.sz = 0 then Printf.printf "EMPTY\n"
          else Printf.printf "%d\n" (min_index pq)
        | 3 ->
          if pq.sz = 0 then Printf.printf "EMPTY\n"
          else Printf.printf "%d\n" (del_min pq)
        | 4 ->
          let i = int_of_string tokens.(!idx) in
          incr idx;
          if has pq i then begin
            erase pq i;
            Printf.printf "OK\n"
          end else
            Printf.printf "NONE\n"
        | 5 ->
          let i = int_of_string tokens.(!idx) in
          incr idx;
          if has pq i then
            Printf.printf "%Ld\n" pq.keys.(i)
          else
            Printf.printf "NONE\n"
        | _ -> ()
      end
    done
  end
