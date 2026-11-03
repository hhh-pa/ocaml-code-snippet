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

type interval = { id : int; start_t : int; end_t : int }
type active_item = { mutable a_end : int; mutable a_id : int; mutable a_reg : int }

module IntSet = Set.Make(Int)

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 2 then (
    let n = int_of_string tokens.(0) in
    let k_regs = int_of_string tokens.(1) in
    let idx = ref 2 in
    let intervals = ref [] in
    for _ = 1 to n do
      if !idx + 2 < Array.length tokens then (
        let id = int_of_string tokens.(!idx) in
        let s = int_of_string tokens.(!idx + 1) in
        let e = int_of_string tokens.(!idx + 2) in
        idx := !idx + 3;
        intervals := { id; start_t = s; end_t = e } :: !intervals
      )
    done;
    let intervals = Array.of_list (List.rev !intervals) in
    let sorted_intervals = Array.copy intervals in
    Array.sort (fun a b ->
      if a.start_t <> b.start_t then compare a.start_t b.start_t
      else compare a.end_t b.end_t
    ) sorted_intervals;

    let free_regs = ref IntSet.empty in
    for r = 0 to k_regs - 1 do
      free_regs := IntSet.add r !free_regs
    done;

    let active = ref [] in
    let assigned_reg = Array.make n (-1) in
    let is_spilled = Array.make n false in

    Array.iter (fun cur ->
      let new_active = ref [] in
      List.iter (fun act ->
        if act.a_end < cur.start_t then
          free_regs := IntSet.add act.a_reg !free_regs
        else
          new_active := act :: !new_active
      ) !active;
      active := List.rev !new_active;

      if List.length !active = k_regs then (
        let latest_act = ref (List.hd !active) in
        List.iter (fun act ->
          if act.a_end > (!latest_act).a_end then latest_act := act
        ) (List.tl !active);

        if (!latest_act).a_end > cur.end_t then (
          let spilled_id = (!latest_act).a_id in
          let reg = (!latest_act).a_reg in
          is_spilled.(spilled_id) <- true;
          assigned_reg.(spilled_id) <- -1;

          let rem_active = List.filter (fun act -> act != !latest_act) !active in
          assigned_reg.(cur.id) <- reg;
          active := { a_end = cur.end_t; a_id = cur.id; a_reg = reg } :: rem_active
        ) else (
          is_spilled.(cur.id) <- true
        )
      ) else (
        let reg = IntSet.min_elt !free_regs in
        free_regs := IntSet.remove reg !free_regs;
        assigned_reg.(cur.id) <- reg;
        active := { a_end = cur.end_t; a_id = cur.id; a_reg = reg } :: !active
      );

      active := List.sort (fun a b -> compare a.a_end b.a_end) !active
    ) sorted_intervals;

    let allocated_cnt = ref 0 in
    let spilled_cnt = ref 0 in
    for i = 0 to n - 1 do
      if is_spilled.(i) then (
        Printf.printf "v%d: SPILL\n" i;
        incr spilled_cnt
      ) else (
        Printf.printf "v%d: R%d\n" i assigned_reg.(i);
        incr allocated_cnt
      )
    done;
    Printf.printf "Allocated: %d Spilled: %d\n" !allocated_cnt !spilled_cnt
  )
