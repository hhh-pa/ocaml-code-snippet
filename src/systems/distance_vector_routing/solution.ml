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
let half_inf = 2000000000000000000L

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 2 then (
    let n = int_of_string tokens.(0) in
    let e = int_of_string tokens.(1) in
    let cost = Array.make_matrix n n inf_ll in
    let nbr = Array.init n (fun _ -> ref []) in
    let idx = ref 2 in
    for _ = 0 to e - 1 do
      let u = int_of_string tokens.(!idx) in
      let v = int_of_string tokens.(!idx + 1) in
      let c = Int64.of_string tokens.(!idx + 2) in
      idx := !idx + 3;
      cost.(u).(v) <- c;
      cost.(v).(u) <- c;
      nbr.(u) := v :: !(nbr.(u));
      nbr.(v) := u :: !(nbr.(v))
    done;
    for i = 0 to n - 1 do
      nbr.(i) := List.rev !(nbr.(i))
    done;

    let dist = ref (Array.make_matrix n n inf_ll) in
    let nx = ref (Array.make_matrix n n (-1)) in
    for i = 0 to n - 1 do
      !dist.(i).(i) <- 0L;
      !nx.(i).(i) <- i;
      List.iter (fun nb ->
        !dist.(i).(nb) <- cost.(i).(nb);
        !nx.(i).(nb) <- nb
      ) !(nbr.(i))
    done;

    let rounds = ref 0 in
    let cont = ref true in
    let iter_count = ref 0 in
    while !iter_count < 1000 && !cont do
      let changed = ref false in
      let nd = Array.init n (fun i -> Array.copy !dist.(i)) in
      let nn = Array.init n (fun i -> Array.copy !nx.(i)) in
      for i = 0 to n - 1 do
        for d = 0 to n - 1 do
          if d <> i then (
            let best = ref inf_ll in
            let bn = ref (-1) in
            List.iter (fun nb ->
              let a = if !nx.(nb).(d) = i && d <> nb then inf_ll else !dist.(nb).(d) in
              let cand = if a >= half_inf then inf_ll else Int64.add cost.(i).(nb) a in
              if cand < !best || (cand = !best && (!bn = -1 || nb < !bn)) then (
                best := cand;
                bn := nb
              )
            ) !(nbr.(i));
            if !best <> !dist.(i).(d) || !bn <> !nx.(i).(d) then (
              nd.(i).(d) <- !best;
              nn.(i).(d) <- !bn;
              changed := true
            )
          )
        done
      done;
      dist := nd;
      nx := nn;
      if not !changed then cont := false
      else incr rounds;
      incr iter_count
    done;

    Printf.printf "%d\n" !rounds;
    let dst = n - 1 in
    for i = 0 to n - 1 do
      if i = dst then print_endline "0 -"
      else if !dist.(i).(dst) >= half_inf then print_endline "INF -"
      else Printf.printf "%Ld %d\n" !dist.(i).(dst) !nx.(i).(dst)
    done
  )
