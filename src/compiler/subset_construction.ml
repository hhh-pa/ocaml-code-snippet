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
module StateSetMap = Map.Make(IntSet)
module CharSet = Set.Make(Char)

type nfa_trans = { ch : char; to_state : int }

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 3 then (
    let n_states = int_of_string tokens.(0) in
    let start_state = int_of_string tokens.(1) in
    let num_accept = int_of_string tokens.(2) in
    let idx = ref 3 in
    let nfa_accept = ref IntSet.empty in
    for _ = 1 to num_accept do
      if !idx < Array.length tokens then (
        nfa_accept := IntSet.add (int_of_string tokens.(!idx)) !nfa_accept;
        incr idx
      )
    done;

    let num_transitions = int_of_string tokens.(!idx) in
    incr idx;
    let nfa = Array.init n_states (fun _ -> ref []) in
    let alphabet_set = ref CharSet.empty in
    for _ = 1 to num_transitions do
      if !idx + 2 < Array.length tokens then (
        let u = int_of_string tokens.(!idx) in
        let v = int_of_string tokens.(!idx + 1) in
        let c = tokens.(!idx + 2).[0] in
        idx := !idx + 3;
        nfa.(u) := { ch = c; to_state = v } :: !(nfa.(u));
        if c <> '#' then alphabet_set := CharSet.add c !alphabet_set
      )
    done;
    let alphabet = CharSet.elements !alphabet_set in

    let epsilon_closure states =
      let closure = ref states in
      let q = Queue.create () in
      IntSet.iter (fun s -> Queue.push s q) states;
      while not (Queue.is_empty q) do
        let u = Queue.pop q in
        List.iter (fun tr ->
          if tr.ch = '#' && not (IntSet.mem tr.to_state !closure) then (
            closure := IntSet.add tr.to_state !closure;
            Queue.push tr.to_state q
          )
        ) !(nfa.(u))
      done;
      !closure
    in

    let move_states states c =
      let res = ref IntSet.empty in
      IntSet.iter (fun s ->
        List.iter (fun tr ->
          if tr.ch = c then res := IntSet.add tr.to_state !res
        ) !(nfa.(s))
      ) states;
      !res
    in

    let start_set = epsilon_closure (IntSet.singleton start_state) in
    let dfa_state_map = ref (StateSetMap.singleton start_set 0) in
    let dfa_states = ref [start_set] in
    let start_acc = IntSet.exists (fun s -> IntSet.mem s !nfa_accept) start_set in
    let dfa_accept = ref [start_acc] in
    let dfa_trans = ref [Hashtbl.create 4] in

    let work_queue = Queue.create () in
    Queue.push 0 work_queue;

    while not (Queue.is_empty work_queue) do
      let curr_id = Queue.pop work_queue in
      let curr_set = List.nth !dfa_states curr_id in
      let curr_trans = List.nth !dfa_trans curr_id in

      List.iter (fun c ->
        let moved = move_states curr_set c in
        let nxt = epsilon_closure moved in
        if not (IntSet.is_empty nxt) then (
          let nxt_id =
            match StateSetMap.find_opt nxt !dfa_state_map with
            | Some id -> id
            | None ->
              let id = List.length !dfa_states in
              dfa_state_map := StateSetMap.add nxt id !dfa_state_map;
              dfa_states := !dfa_states @ [nxt];
              let is_acc = IntSet.exists (fun s -> IntSet.mem s !nfa_accept) nxt in
              dfa_accept := !dfa_accept @ [is_acc];
              dfa_trans := !dfa_trans @ [Hashtbl.create 4];
              Queue.push id work_queue;
              id
          in
          Hashtbl.replace curr_trans c nxt_id
        )
      ) alphabet
    done;

    let num_dfa_states = List.length !dfa_states in
    let dfa_acc_arr = Array.of_list !dfa_accept in
    let dfa_trans_arr = Array.of_list !dfa_trans in

    let acc_count = ref 0 in
    Array.iter (fun b -> if b then incr acc_count) dfa_acc_arr;

    Printf.printf "DFA states: %d\n" num_dfa_states;
    Printf.printf "DFA accept states: %d\n" !acc_count;

    if !idx < Array.length tokens then (
      let num_queries = int_of_string tokens.(!idx) in
      incr idx;
      for _ = 1 to num_queries do
        if !idx < Array.length tokens then (
          let w_str = tokens.(!idx) in
          incr idx;
          let w = if w_str = "EPS" then "" else w_str in
          let state = ref 0 in
          let ok = ref true in
          let len = String.length w in
          let i = ref 0 in
          while !ok && !i < len do
            let ch = w.[!i] in
            match Hashtbl.find_opt dfa_trans_arr.(!state) ch with
            | Some nxt -> state := nxt
            | None -> ok := false
          ;
          incr i
        done;
        if !ok && dfa_acc_arr.(!state) then
          Printf.printf "ACCEPT\n"
        else
          Printf.printf "REJECT\n"
      )
    done
  )
)
