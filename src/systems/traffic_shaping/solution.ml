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

let rec gcd a b =
  if b = 0 then abs a
  else gcd b (a mod b)

let lcm a b =
  if a = 0 || b = 0 then 0
  else (a / gcd a b) * b

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 1 then (
    let policy = int_of_string tokens.(0) in
    let idx = ref 1 in
    if policy = 0 then (
      let rate = int_of_string tokens.(!idx) in
      let depth = int_of_string tokens.(!idx + 1) in
      let n = int_of_string tokens.(!idx + 2) in
      idx := !idx + 3;
      let toks = ref depth in
      let last = ref 0 in
      let first = ref true in
      for _ = 0 to n - 1 do
        let t = int_of_string tokens.(!idx) in
        let s = int_of_string tokens.(!idx + 1) in
        idx := !idx + 2;
        if !first then (
          last := t;
          first := false
        );
        toks := min depth (!toks + rate * (t - !last));
        last := t;
        if !toks >= s then (
          toks := !toks - s;
          print_endline "CONFORM"
        ) else (
          print_endline "DROP"
        )
      done
    ) else if policy = 1 then (
      let rate = int_of_string tokens.(!idx) in
      let depth = int_of_string tokens.(!idx + 1) in
      let n = int_of_string tokens.(!idx + 2) in
      idx := !idx + 3;
      let content = ref 0 in
      let last = ref 0 in
      let first = ref true in
      for _ = 0 to n - 1 do
        let t = int_of_string tokens.(!idx) in
        let s = int_of_string tokens.(!idx + 1) in
        idx := !idx + 2;
        if !first then (
          last := t;
          first := false
        );
        content := max 0 (!content - rate * (t - !last));
        last := t;
        if !content + s <= depth then (
          content := !content + s;
          print_endline "CONFORM"
        ) else (
          print_endline "DROP"
        )
      done
    ) else (
      let num_f = int_of_string tokens.(!idx) in
      let n = int_of_string tokens.(!idx + 1) in
      idx := !idx + 2;
      let w = Array.init num_f (fun _ ->
        let v = int_of_string tokens.(!idx) in
        incr idx;
        v
      ) in
      let l_val = ref 1 in
      for i = 0 to num_f - 1 do
        l_val := lcm !l_val w.(i)
      done;
      let cap_l = !l_val in

      let pk = Array.init n (fun _ ->
        let flow = int_of_string tokens.(!idx) in
        let arr = int_of_string tokens.(!idx + 1) in
        let size = int_of_string tokens.(!idx + 2) in
        idx := !idx + 3;
        (flow, arr, size)
      ) in

      let prev_f = Array.make num_f 0 in
      let fnum = Array.make n 0 in
      for i = 0 to n - 1 do
        let (flow, arr, size) = pk.(i) in
        let start = max (arr * cap_l) prev_f.(flow) in
        let fin = start + size * (cap_l / w.(flow)) in
        prev_f.(flow) <- fin;
        fnum.(i) <- fin
      done;

      for i = 0 to n - 1 do
        Printf.printf "%d/%d\n" fnum.(i) cap_l
      done;

      let ord_idx = Array.init n (fun i -> i) in
      Array.sort (fun a b ->
        if fnum.(a) <> fnum.(b) then compare fnum.(a) fnum.(b)
        else
          let (fa, _, _) = pk.(a) in
          let (fb, _, _) = pk.(b) in
          if fa <> fb then compare fa fb
          else compare a b
      ) ord_idx;

      let ord_strs = Array.to_list (Array.map string_of_int ord_idx) in
      print_endline ("ORDER " ^ String.concat " " ord_strs)
    )
  )
