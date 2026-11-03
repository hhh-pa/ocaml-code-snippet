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

type cw_node = {
  next : int array;
  mutable fail : int;
  depth : int;
  mutable out : int list;
}

let create_node d = {
  next = Array.make 26 (-1);
  fail = 0;
  depth = d;
  out = [];
}

let commentz_walter patterns text =
  let num_patterns = Array.length patterns in
  let results = Array.init num_patterns (fun _ -> ref []) in
  if num_patterns = 0 || String.length text = 0 then
    Array.map (fun r -> List.rev !r) results
  else
    let w_min = ref (String.length patterns.(0)) in
    for i = 1 to num_patterns - 1 do
      let l = String.length patterns.(i) in
      if l < !w_min then w_min := l
    done;
    let w_min = !w_min in
    let text_len = String.length text in
    if w_min = 0 || w_min > text_len then
      Array.map (fun r -> List.rev !r) results
    else begin
      let trie = ref (Array.make 16 (create_node 0)) in
      !trie.(0) <- create_node 0;
      let trie_size = ref 1 in
      let add_node d =
        if !trie_size = Array.length !trie then (
          let old = !trie in
          let nw = Array.make (Array.length old * 2) (create_node 0) in
          Array.blit old 0 nw 0 !trie_size;
          trie := nw
        );
        let id = !trie_size in
        !trie.(id) <- create_node d;
        incr trie_size;
        id
      in
      let char_min_depth = Array.make 26 (w_min + 1) in

      for idx = 0 to num_patterns - 1 do
        let pat = patterns.(idx) in
        let pat_len = String.length pat in
        let u = ref 0 in
        for k = 0 to pat_len - 1 do
          let ch = pat.[pat_len - 1 - k] in
          let c = Char.code ch - Char.code 'a' in
          let dep = k + 1 in
          if dep >= 2 && dep < char_min_depth.(c) then
            char_min_depth.(c) <- dep;
          if !trie.(!u).next.(c) = -1 then (
            let next_id = add_node dep in
            !trie.(!u).next.(c) <- next_id
          );
          u := !trie.(!u).next.(c)
        done;
        !trie.(!u).out <- idx :: !trie.(!u).out
      done;

      for i = 0 to !trie_size - 1 do
        !trie.(i).out <- List.rev !trie.(i).out
      done;

      let q = Queue.create () in
      for c = 0 to 25 do
        let nxt = !trie.(0).next.(c) in
        if nxt <> -1 then (
          !trie.(nxt).fail <- 0;
          Queue.push nxt q
        )
      done;
      while not (Queue.is_empty q) do
        let u = Queue.pop q in
        for c = 0 to 25 do
          let v = !trie.(u).next.(c) in
          if v <> -1 then (
            let f = ref !trie.(u).fail in
            while !f > 0 && !trie.(!f).next.(c) = -1 do
              f := !trie.(!f).fail
            done;
            if !trie.(!f).next.(c) <> -1 then
              f := !trie.(!f).next.(c);
            !trie.(v).fail <- !f;
            Queue.push v q
          )
        done
      done;

      let n = text_len in
      let j = ref (w_min - 1) in
      while !j < n do
        let u = ref 0 in
        let pos = ref !j in
        let cont = ref true in
        while !cont do
          let c = Char.code text.[!pos] - Char.code 'a' in
          let nxt = !trie.(!u).next.(c) in
          if nxt = -1 then cont := false
          else (
            u := nxt;
            List.iter (fun pat_idx ->
              results.(pat_idx) := !pos :: !(results.(pat_idx))
            ) !trie.(!u).out;
            if !pos = 0 then cont := false
            else decr pos
          )
        done;
        let c_right = Char.code text.[!j] - Char.code 'a' in
        let shift = ref (char_min_depth.(c_right) - 1) in
        if !shift < 1 then shift := 1;
        j := !j + !shift
      done;

      Array.map (fun r -> List.rev !r) results
    end

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 1 then (
    let n = int_of_string tokens.(0) in
    let patterns = Array.init n (fun i -> tokens.(i + 1)) in
    let text = tokens.(n + 1) in
    let res = commentz_walter patterns text in
    for i = 0 to n - 1 do
      let matches = res.(i) in
      if matches = [] then
        print_endline "-1"
      else (
        let strs = List.map string_of_int matches in
        print_endline (String.concat " " strs)
      )
    done
  )
