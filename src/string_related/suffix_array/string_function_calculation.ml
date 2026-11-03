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

let cal_suffix_array s =
  let n = String.length s in
  let sa = Array.init n (fun i -> i) in
  let rank = ref (Array.init n (fun i -> Char.code s.[i] - Char.code 'a')) in
  let tmp = ref (Array.make n 0) in
  let k = ref 1 in
  while !k < n do
    let cur_k = !k in
    let r = !rank in
    let cmp a b =
      let ra = r.(a) in
      let rb = r.(b) in
      if ra <> rb then compare ra rb
      else
        let ra2 = if a + cur_k < n then r.(a + cur_k) else -1 in
        let rb2 = if b + cur_k < n then r.(b + cur_k) else -1 in
        compare ra2 rb2
    in
    Array.sort cmp sa;
    let curr_rank = ref 0 in
    !tmp.(sa.(0)) <- 0;
    for i = 1 to n - 1 do
      let prev = sa.(i - 1) in
      let curr = sa.(i) in
      let diff =
        r.(prev) <> r.(curr) ||
        (if prev + cur_k < n then r.(prev + cur_k) else -1) <>
        (if curr + cur_k < n then r.(curr + cur_k) else -1)
      in
      if diff then incr curr_rank;
      !tmp.(curr) <- !curr_rank
    done;
    let t = !rank in
    rank := !tmp;
    tmp := t;
    if !rank.(sa.(n - 1)) = n - 1 then k := n
    else k := !k * 2
  done;
  sa

let kasai s sa =
  let n = String.length s in
  let rank = Array.make n 0 in
  for i = 0 to n - 1 do
    rank.(sa.(i)) <- i
  done;
  let lcp = Array.make n 0 in
  let k = ref 0 in
  for i = 0 to n - 1 do
    if rank.(i) = n - 1 then (
      k := 0
    ) else (
      let j = sa.(rank.(i) + 1) in
      while i + !k < n && j + !k < n && s.[i + !k] = s.[j + !k] do
        incr k
      done;
      lcp.(rank.(i)) <- !k;
      if !k > 0 then decr k
    )
  done;
  lcp

let max_value s =
  let n = String.length s in
  if n = 0 then 0L
  else
    let sa = cal_suffix_array s in
    let lcp = kasai s sa in
    let ans = ref (Int64.of_int n) in
    let prev_less = Array.make n (-1) in
    let stack = ref [] in
    for i = 0 to n - 1 do
      while !stack <> [] && lcp.(List.hd !stack) >= lcp.(i) do
        stack := List.tl !stack
      done;
      prev_less.(i) <- (if !stack = [] then -1 else List.hd !stack);
      stack := i :: !stack
    done;
    let next_less = Array.make n n in
    stack := [];
    for i = n - 1 downto 0 do
      while !stack <> [] && lcp.(List.hd !stack) >= lcp.(i) do
        stack := List.tl !stack
      done;
      next_less.(i) <- (if !stack = [] then n else List.hd !stack);
      stack := i :: !stack
    done;
    for i = 0 to n - 1 do
      let cnt = Int64.of_int (next_less.(i) - prev_less.(i)) in
      let v = Int64.mul cnt (Int64.of_int lcp.(i)) in
      if v > !ans then ans := v
    done;
    !ans

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 1 then (
    let s = tokens.(0) in
    Printf.printf "%Ld\n" (max_value s)
  )
