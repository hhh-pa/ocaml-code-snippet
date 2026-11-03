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

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 2 then (
    let f = int_of_string tokens.(0) in
    let n = int_of_string tokens.(1) in
    let refs = Array.init n (fun i -> int_of_string tokens.(2 + i)) in

    (* OPT *)
    let opt = ref 0 in
    let frames_opt = ref [] in
    for i = 0 to n - 1 do
      let r = refs.(i) in
      if not (List.mem r !frames_opt) then (
        incr opt;
        if List.length !frames_opt < f then
          frames_opt := !frames_opt @ [r]
        else (
          let arr = Array.of_list !frames_opt in
          let victim = ref 0 in
          let far = ref (-1) in
          for s = 0 to Array.length arr - 1 do
            let p = arr.(s) in
            let nx = ref (n + 1) in
            let j = ref (i + 1) in
            while !j < n && !nx = n + 1 do
              if refs.(!j) = p then nx := !j;
              incr j
            done;
            if !nx > !far then (
              far := !nx;
              victim := s
            )
          done;
          arr.(!victim) <- r;
          frames_opt := Array.to_list arr
        )
      )
    done;

    (* FIFO *)
    let fifo = ref 0 in
    let frames_fifo = ref [] in
    let head = ref 0 in
    for i = 0 to n - 1 do
      let r = refs.(i) in
      if not (List.mem r !frames_fifo) then (
        incr fifo;
        if List.length !frames_fifo < f then
          frames_fifo := !frames_fifo @ [r]
        else (
          let arr = Array.of_list !frames_fifo in
          arr.(!head) <- r;
          head := (!head + 1) mod (Array.length arr);
          frames_fifo := Array.to_list arr
        )
      )
    done;

    (* Clock *)
    let clock = ref 0 in
    let frames_clk = Array.make f 0 in
    let occ = Array.make f false in
    let ref_bit = Array.make f false in
    let hand = ref 0 in
    for i = 0 to n - 1 do
      let r = refs.(i) in
      let hit = ref false in
      for s = 0 to f - 1 do
        if occ.(s) && frames_clk.(s) = r then (
          hit := true;
          ref_bit.(s) <- true
        )
      done;
      if not !hit then (
        incr clock;
        let searching = ref true in
        while !searching do
          if not occ.(!hand) then
            searching := false
          else if ref_bit.(!hand) then (
            ref_bit.(!hand) <- false;
            hand := (!hand + 1) mod f
          ) else
            searching := false
        done;
        let h = !hand in
        frames_clk.(h) <- r;
        occ.(h) <- true;
        ref_bit.(h) <- true;
        hand := (!hand + 1) mod f
      )
    done;

    (* LRU *)
    let lru = ref 0 in
    let frames_lru = ref [] in
    for i = 0 to n - 1 do
      let r = refs.(i) in
      if List.mem r !frames_lru then (
        frames_lru := (remove_first r !frames_lru) @ [r]
      ) else (
        incr lru;
        if List.length !frames_lru < f then
          frames_lru := !frames_lru @ [r]
        else
          frames_lru := (List.tl !frames_lru) @ [r]
      )
    done;

    Printf.printf "%d %d %d %d\n" !opt !fifo !clock !lru
  )
