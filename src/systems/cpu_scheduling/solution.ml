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
    let policy = int_of_string tokens.(0) in
    let idx_tok = ref 1 in
    let quantum =
      if policy = 2 then (
        let q = int_of_string tokens.(!idx_tok) in
        incr idx_tok;
        q
      ) else 1
    in
    let n = int_of_string tokens.(!idx_tok) in
    incr idx_tok;
    let arr = Array.make n 0 in
    let burst = Array.make n 0 in
    for i = 0 to n - 1 do
      arr.(i) <- int_of_string tokens.(!idx_tok);
      burst.(i) <- int_of_string tokens.(!idx_tok + 1);
      idx_tok := !idx_tok + 2
    done;

    let order = ref [] in
    let comp = Array.make n (-1) in
    let idx = Array.init n (fun i -> i) in
    Array.sort (fun a b ->
      if arr.(a) <> arr.(b) then compare arr.(a) arr.(b)
      else compare a b
    ) idx;

    if policy = 0 then (
      let now = ref 0 in
      for k = 0 to n - 1 do
        let i = idx.(k) in
        if !now < arr.(i) then now := arr.(i);
        now := !now + burst.(i);
        comp.(i) <- !now;
        order := i :: !order
      done
    ) else if policy = 1 then (
      let done_arr = Array.make n false in
      let now = ref 0 in
      let left = ref n in
      while !left > 0 do
        let pick = ref (-1) in
        for i = 0 to n - 1 do
          if not done_arr.(i) && arr.(i) <= !now then (
            if !pick = -1 || burst.(i) < burst.(!pick) ||
               (burst.(i) = burst.(!pick) && (arr.(i) < arr.(!pick) || (arr.(i) = arr.(!pick) && i < !pick))) then
              pick := i
          )
        done;
        if !pick = -1 then (
          let min_arr = ref 1000000000 in
          for i = 0 to n - 1 do
            if not done_arr.(i) && arr.(i) < !min_arr then min_arr := arr.(i)
          done;
          now := !min_arr
        ) else (
          let p = !pick in
          now := !now + burst.(p);
          comp.(p) <- !now;
          done_arr.(p) <- true;
          order := p :: !order;
          decr left
        )
      done
    ) else (
      let rem = Array.copy burst in
      let ready = Queue.create () in
      let now = ref 0 in
      let ap = ref 0 in
      let left = ref n in
      while !left > 0 do
        while !ap < n && arr.(idx.(!ap)) <= !now do
          Queue.push idx.(!ap) ready;
          incr ap
        done;
        if Queue.is_empty ready then (
          now := arr.(idx.(!ap))
        ) else (
          let id = Queue.pop ready in
          let run = min rem.(id) quantum in
          now := !now + run;
          rem.(id) <- rem.(id) - run;
          while !ap < n && arr.(idx.(!ap)) <= !now do
            Queue.push idx.(!ap) ready;
            incr ap
          done;
          if rem.(id) > 0 then
            Queue.push id ready
          else (
            comp.(id) <- !now;
            order := id :: !order;
            decr left
          )
        )
      done
    );

    let res_order = List.rev !order in
    let strs = List.map string_of_int res_order in
    print_endline (String.concat " " strs);
    let tot = ref 0 in
    for i = 0 to n - 1 do
      tot := !tot + (comp.(i) - arr.(i) - burst.(i))
    done;
    Printf.printf "%d\n" !tot
  )
