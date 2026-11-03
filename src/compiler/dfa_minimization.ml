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

module IntSet = Set.Make(Int)

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 3 then (
    let n = int_of_string tokens.(0) in
    let start_state = int_of_string tokens.(1) in
    let num_accept = int_of_string tokens.(2) in
    let idx = ref 3 in
    let accept_states = ref IntSet.empty in
    for _ = 1 to num_accept do
      accept_states := IntSet.add (int_of_string tokens.(!idx)) !accept_states;
      incr idx
    done;
    let alpha_size = int_of_string tokens.(!idx) in
    incr idx;
    let alphabet = Array.init alpha_size (fun _ ->
      let c = tokens.(!idx).[0] in
      incr idx;
      c
    ) in
    let num_trans = int_of_string tokens.(!idx) in
    incr idx;

    let trans = Array.init n (fun _ -> Hashtbl.create 4) in
    let rev_trans = Array.init n (fun _ -> Hashtbl.create 4) in
    for _ = 1 to num_trans do
      let u = int_of_string tokens.(!idx) in
      let v = int_of_string tokens.(!idx + 1) in
      let c = tokens.(!idx + 2).[0] in
      idx := !idx + 3;
      Hashtbl.replace trans.(u) c v;
      let prev = match Hashtbl.find_opt rev_trans.(v) c with Some lst -> lst | None -> [] in
      Hashtbl.replace rev_trans.(v) c (u :: prev)
    done;

    let reachable = Array.make n false in
    let q = Queue.create () in
    reachable.(start_state) <- true;
    Queue.push start_state q;
    while not (Queue.is_empty q) do
      let u = Queue.pop q in
      Hashtbl.iter (fun _ v ->
        if not reachable.(v) then (
          reachable.(v) <- true;
          Queue.push v q
        )
      ) trans.(u)
    done;

    let states = ref [] in
    for i = 0 to n - 1 do
      if reachable.(i) then states := i :: !states
    done;
    let states = List.rev !states in

    let set_f = ref IntSet.empty in
    let set_non_f = ref IntSet.empty in
    List.iter (fun s ->
      if IntSet.mem s !accept_states then
        set_f := IntSet.add s !set_f
      else
        set_non_f := IntSet.add s !set_non_f
    ) states;

    let p = ref [] in
    if not (IntSet.is_empty !set_f) then p := !set_f :: !p;
    if not (IntSet.is_empty !set_non_f) then p := !set_non_f :: !p;
    p := List.rev !p;

    let w = Queue.create () in
    if not (IntSet.is_empty !set_f) && not (IntSet.is_empty !set_non_f) then (
      if IntSet.cardinal !set_f <= IntSet.cardinal !set_non_f then
        Queue.push !set_f w
      else
        Queue.push !set_non_f w
    ) else if not (IntSet.is_empty !set_f) then
      Queue.push !set_f w
    else if not (IntSet.is_empty !set_non_f) then
      Queue.push !set_non_f w;

    while not (Queue.is_empty w) do
      let a = Queue.pop w in
      Array.iter (fun c ->
        let x = ref IntSet.empty in
        IntSet.iter (fun s ->
          match Hashtbl.find_opt rev_trans.(s) c with
          | Some u_list ->
            List.iter (fun u ->
              if reachable.(u) then x := IntSet.add u !x
            ) u_list
          | None -> ()
        ) a;

        if not (IntSet.is_empty !x) then (
          let new_p = ref [] in
          List.iter (fun y ->
            let inter = IntSet.inter y !x in
            let diff = IntSet.diff y !x in
            if not (IntSet.is_empty inter) && not (IntSet.is_empty diff) then (
              new_p := diff :: inter :: !new_p;
              let next_w = Queue.create () in
              let in_w = ref false in
              while not (Queue.is_empty w) do
                let item = Queue.pop w in
                if IntSet.equal item y then (
                  in_w := true;
                  Queue.push inter next_w;
                  Queue.push diff next_w
                ) else
                  Queue.push item next_w
              done;
              Queue.transfer next_w w;
              if not !in_w then (
                if IntSet.cardinal inter <= IntSet.cardinal diff then
                  Queue.push inter w
                else
                  Queue.push diff w
              )
            ) else
              new_p := y :: !new_p
          ) !p;
          p := List.rev !new_p
        )
      ) alphabet
    done;

    let p_arr = Array.of_list !p in
    let state_to_part = Array.make n (-1) in
    Array.iteri (fun pid part ->
      IntSet.iter (fun s -> state_to_part.(s) <- pid) part
    ) p_arr;

    let min_start = state_to_part.(start_state) in
    let min_accept = ref IntSet.empty in
    Array.iteri (fun pid part ->
      if IntSet.exists (fun s -> IntSet.mem s !accept_states) part then
        min_accept := IntSet.add pid !min_accept
    ) p_arr;

    Printf.printf "Minimal DFA states: %d\n" (Array.length p_arr);
    Printf.printf "Accept states: %d\n" (IntSet.cardinal !min_accept);

    let min_trans = Array.init (Array.length p_arr) (fun _ -> Hashtbl.create 4) in
    Array.iteri (fun pid part ->
      let rep = IntSet.min_elt part in
      Hashtbl.iter (fun c v ->
        if reachable.(v) then
          Hashtbl.replace min_trans.(pid) c state_to_part.(v)
      ) trans.(rep)
    ) p_arr;

    if !idx < Array.length tokens then (
      let num_queries = int_of_string tokens.(!idx) in
      incr idx;
      for _ = 1 to num_queries do
        if !idx < Array.length tokens then (
          let w_str = tokens.(!idx) in
          incr idx;
          let w = if w_str = "EPS" then "" else w_str in
          let curr = ref min_start in
          let ok = ref true in
          let len = String.length w in
          let i = ref 0 in
          while !ok && !i < len do
            let ch = w.[!i] in
            if !curr < 0 || !curr >= Array.length min_trans then
              ok := false
            else (
              match Hashtbl.find_opt min_trans.(!curr) ch with
              | Some nxt -> curr := nxt
              | None -> ok := false
            );
            incr i
          done;
          if !ok && IntSet.mem !curr !min_accept then
            Printf.printf "ACCEPT\n"
          else
            Printf.printf "REJECT\n"
        )
      done
    )
  )
