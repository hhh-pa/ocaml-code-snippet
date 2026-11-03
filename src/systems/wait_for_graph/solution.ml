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
    let n = int_of_string tokens.(0) in
    let m = int_of_string tokens.(1) in
    let g = Array.init n (fun _ -> ref []) in
    let gr = Array.init n (fun _ -> ref []) in
    let self_loop = Array.make n false in
    let idx = ref 2 in
    for _ = 0 to m - 1 do
      let u = int_of_string tokens.(!idx) in
      let v = int_of_string tokens.(!idx + 1) in
      idx := !idx + 2;
      if u >= 0 && u < n && v >= 0 && v < n then (
        g.(u) := v :: !(g.(u));
        gr.(v) := u :: !(gr.(v));
        if u = v then self_loop.(u) <- true
      )
    done;
    for i = 0 to n - 1 do
      g.(i) := List.rev !(g.(i));
      gr.(i) := List.rev !(gr.(i))
    done;

    let seen = Array.make n false in
    let order = ref [] in
    for s = 0 to n - 1 do
      if not seen.(s) then (
        let st = ref [(s, !(g.(s)))] in
        seen.(s) <- true;
        while !st <> [] do
          match !st with
          | (u, []) :: rest ->
            order := u :: !order;
            st := rest
          | (u, v :: vs) :: rest ->
            st := (u, vs) :: rest;
            if not seen.(v) then (
              seen.(v) <- true;
              st := (v, !(g.(v))) :: !st
            )
          | [] -> ()
        done
      )
    done;

    let comp = Array.make n (-1) in
    let nc = ref 0 in
    List.iter (fun s ->
      if comp.(s) = -1 then (
        let cur_c = !nc in
        comp.(s) <- cur_c;
        let st = ref [s] in
        while !st <> [] do
          let u = List.hd !st in
          st := List.tl !st;
          List.iter (fun v ->
            if comp.(v) = -1 then (
              comp.(v) <- cur_c;
              st := v :: !st
            )
          ) !(gr.(u))
        done;
        incr nc
      )
    ) !order;

    let size = Array.make !nc 0 in
    for i = 0 to n - 1 do
      size.(comp.(i)) <- size.(comp.(i)) + 1
    done;

    let cyc = ref [] in
    for u = 0 to n - 1 do
      if size.(comp.(u)) > 1 || self_loop.(u) then
        cyc := u :: !cyc
    done;
    let res = List.rev !cyc in
    if res = [] then print_endline "NO DEADLOCK"
    else (
      let strs = List.map string_of_int res in
      print_endline ("DEADLOCK " ^ String.concat " " strs)
    )
  )
