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

module StringSet = Set.Make(String)

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 1 then (
    let n = int_of_string tokens.(0) in
    let idx = ref 1 in
    let ch = Array.init n (fun _ -> ref []) in
    for _ = 0 to n - 2 do
      let p = int_of_string tokens.(!idx) in
      let c = int_of_string tokens.(!idx + 1) in
      idx := !idx + 2;
      ch.(p) := c :: !(ch.(p))
    done;
    for i = 0 to n - 1 do
      ch.(i) := List.rev !(ch.(i))
    done;

    let num_l = int_of_string tokens.(!idx) in
    incr idx;
    let leaf = Array.make n "" in
    let alpha_set = ref StringSet.empty in
    for _ = 0 to num_l - 1 do
      let id_ = int_of_string tokens.(!idx) in
      let st = tokens.(!idx + 1) in
      idx := !idx + 2;
      leaf.(id_) <- st;
      alpha_set := StringSet.add st !alpha_set
    done;
    let alpha = Array.of_list (StringSet.elements !alpha_set) in
    let na = Array.length alpha in

    let order = ref [] in
    let seen = Array.make n false in
    let st = ref [0] in
    seen.(0) <- true;
    while !st <> [] do
      let u = List.hd !st in
      st := List.tl !st;
      order := u :: !order;
      List.iter (fun v ->
        if not seen.(v) then (
          seen.(v) <- true;
          st := v :: !st
        )
      ) !(ch.(u))
    done;
    let order_arr = Array.of_list !order in
    let order_len = Array.length order_arr in

    let fs = Array.init n (fun _ -> ref StringSet.empty) in
    let fcost = ref 0 in
    for k = 0 to order_len - 1 do
      let u = order_arr.(k) in
      if !(ch.(u)) = [] then (
        fs.(u) := StringSet.singleton leaf.(u)
      ) else (
        let best = ref 1000000000 in
        let bs = ref StringSet.empty in
        for a_idx = 0 to na - 1 do
          let a = alpha.(a_idx) in
          let mm = ref 0 in
          List.iter (fun v ->
            if not (StringSet.mem a !(fs.(v))) then incr mm
          ) !(ch.(u));
          if !mm < !best then (
            best := !mm;
            bs := StringSet.singleton a
          ) else if !mm = !best then (
            bs := StringSet.add a !bs
          )
        done;
        fs.(u) := !bs;
        fcost := !fcost + !best
      )
    done;

    let inf = 1000000000 in
    let ai = Hashtbl.create na in
    for i = 0 to na - 1 do
      Hashtbl.add ai alpha.(i) i
    done;
    let sc = Array.make_matrix n na inf in

    for k = 0 to order_len - 1 do
      let u = order_arr.(k) in
      if !(ch.(u)) = [] then (
        let leaf_idx = Hashtbl.find ai leaf.(u) in
        sc.(u).(leaf_idx) <- 0
      ) else (
        for a = 0 to na - 1 do
          let tot = ref 0 in
          List.iter (fun v ->
            let best = ref inf in
            for b = 0 to na - 1 do
              let v_val = sc.(v).(b) + (if a = b then 0 else 1) in
              if v_val < !best then best := v_val
            done;
            tot := !tot + !best
          ) !(ch.(u));
          sc.(u).(a) <- !tot
        done
      )
    done;

    let scost = ref inf in
    for a = 0 to na - 1 do
      if sc.(0).(a) < !scost then scost := sc.(0).(a)
    done;

    Printf.printf "%d %d\n" !fcost !scost;
    let root_fs = StringSet.elements !(fs.(0)) in
    print_endline (String.concat "" root_fs)
  )
