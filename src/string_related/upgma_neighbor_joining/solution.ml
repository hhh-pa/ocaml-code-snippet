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
  if Array.length tokens >= 1 then (
    let n = int_of_string tokens.(0) in
    let cap_n = 2 * n in
    let d0 = Array.make_matrix cap_n cap_n 0.0 in
    let idx = ref 1 in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        d0.(i).(j) <- float_of_string tokens.(!idx);
        incr idx
      done
    done;

    (* UPGMA *)
    let d = Array.make_matrix cap_n cap_n 0.0 in
    for i = 0 to cap_n - 1 do
      for j = 0 to cap_n - 1 do
        d.(i).(j) <- d0.(i).(j)
      done
    done;
    let sz = Array.make cap_n 1 in
    let on = Array.make cap_n false in
    for i = 0 to n - 1 do on.(i) <- true done;
    let cnt = ref n in
    let nid = ref n in

    while !cnt > 1 do
      let bi = ref (-1) in
      let bj = ref (-1) in
      let bd = ref 0.0 in
      let first = ref true in
      for i = 0 to !nid - 1 do
        if on.(i) then (
          for j = i + 1 to !nid - 1 do
            if on.(j) then (
              if !first || d.(i).(j) < !bd then (
                first := false;
                bd := d.(i).(j);
                bi := i;
                bj := j
              )
            )
          done
        )
      done;
      let i_min = !bi in
      let j_min = !bj in
      Printf.printf "%d %d\n" i_min j_min;
      let sz_sum = sz.(i_min) + sz.(j_min) in
      let sz_sum_f = float_of_int sz_sum in
      for k = 0 to !nid - 1 do
        if on.(k) && k <> i_min && k <> j_min then (
          let v = (float_of_int sz.(i_min) *. d.(i_min).(k) +. float_of_int sz.(j_min) *. d.(j_min).(k)) /. sz_sum_f in
          d.(!nid).(k) <- v;
          d.(k).(!nid) <- v
        )
      done;
      sz.(!nid) <- sz_sum;
      on.(i_min) <- false;
      on.(j_min) <- false;
      on.(!nid) <- true;
      incr nid;
      decr cnt
    done;

    (* Neighbor-Joining *)
    for i = 0 to cap_n - 1 do
      for j = 0 to cap_n - 1 do
        d.(i).(j) <- d0.(i).(j)
      done
    done;
    for i = 0 to cap_n - 1 do on.(i) <- false done;
    for i = 0 to n - 1 do on.(i) <- true done;
    cnt := n;
    nid := n;
    let r = Array.make cap_n 0.0 in

    while !cnt > 2 do
      for i = 0 to !nid - 1 do
        if on.(i) then (
          let s = ref 0.0 in
          for k = 0 to !nid - 1 do
            if on.(k) then s := !s +. d.(i).(k)
          done;
          r.(i) <- !s
        )
      done;

      let bi = ref (-1) in
      let bj = ref (-1) in
      let bq = ref 0.0 in
      let first = ref true in
      let cnt_m2 = float_of_int (!cnt - 2) in
      for i = 0 to !nid - 1 do
        if on.(i) then (
          for j = i + 1 to !nid - 1 do
            if on.(j) then (
              let q = cnt_m2 *. d.(i).(j) -. r.(i) -. r.(j) in
              if !first || q < !bq then (
                first := false;
                bq := q;
                bi := i;
                bj := j
              )
            )
          done
        )
      done;
      let i_min = !bi in
      let j_min = !bj in
      Printf.printf "%d %d\n" i_min j_min;
      for k = 0 to !nid - 1 do
        if on.(k) && k <> i_min && k <> j_min then (
          let v = (d.(i_min).(k) +. d.(j_min).(k) -. d.(i_min).(j_min)) /. 2.0 in
          d.(!nid).(k) <- v;
          d.(k).(!nid) <- v
        )
      done;
      on.(i_min) <- false;
      on.(j_min) <- false;
      on.(!nid) <- true;
      incr nid;
      decr cnt
    done;

    let a = ref (-1) in
    let b = ref (-1) in
    for i = 0 to !nid - 1 do
      if on.(i) then (
        if !a < 0 then a := i
        else b := i
      )
    done;
    Printf.printf "%d %d\n" !a !b
  )
