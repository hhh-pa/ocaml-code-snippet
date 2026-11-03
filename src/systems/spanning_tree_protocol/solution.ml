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

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 2 then (
    let num_b = int_of_string tokens.(0) in
    let num_e = int_of_string tokens.(1) in
    let idx = ref 2 in
    let links = Array.init num_e (fun _ ->
      let u = int_of_string tokens.(!idx) in
      let v = int_of_string tokens.(!idx + 1) in
      let c = Int64.of_string tokens.(!idx + 2) in
      idx := !idx + 3;
      (u, v, c)
    ) in

    let adj = Array.init num_b (fun _ -> ref []) in
    for i = 0 to num_e - 1 do
      let (u, v, c) = links.(i) in
      adj.(u) := (v, c) :: !(adj.(u));
      adj.(v) := (u, c) :: !(adj.(v))
    done;

    let root = 0 in
    let dist = Array.make num_b inf_ll in
    let done_arr = Array.make num_b false in
    dist.(root) <- 0L;

    for _ = 0 to num_b - 1 do
      let u = ref (-1) in
      for i = 0 to num_b - 1 do
        if not done_arr.(i) && (!u = -1 || dist.(i) < dist.(!u)) then
          u := i
      done;
      let p = !u in
      if p = -1 || dist.(p) = inf_ll then ()
      else (
        done_arr.(p) <- true;
        let dist_p = dist.(p) in
        List.iter (fun (v, c) ->
          let cand = Int64.add dist_p c in
          if cand < dist.(v) then
            dist.(v) <- cand
        ) !(adj.(p))
      )
    done;

    Printf.printf "%d\n" root;
    for b = 0 to num_b - 1 do
      if b = root then
        print_endline "-1"
      else if dist.(b) = inf_ll then
        print_endline "-2"
      else (
        let best = ref (-1) in
        List.iter (fun (nb, c) ->
          if Int64.add dist.(nb) c = dist.(b) && (!best = -1 || nb < !best) then
            best := nb
        ) !(adj.(b));
        Printf.printf "%d\n" !best
      )
    done;

    for i = 0 to num_e - 1 do
      let (u, v, _c) = links.(i) in
      let d =
        if dist.(u) < dist.(v) then u
        else if dist.(v) < dist.(u) then v
        else min u v
      in
      Printf.printf "%d\n" d
    done
  )
