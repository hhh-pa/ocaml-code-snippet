let mo = 1000000007L

type ast =
  | Char of int
  | Concat of ast list
  | Union of ast * ast
  | Star of ast

let parse_regex s =
  let i = ref 0 in
  let n = String.length s in
  let peek () = if !i < n then s.[!i] else '\000' in
  let eat () = let c = peek () in if !i < n then incr i; c in
  let rec parse_expr () =
    let node = ref (parse_term ()) in
    while peek () = '|' do
      ignore (eat ());
      let rhs = parse_term () in
      node := Union (!node, rhs)
    done;
    !node
  and parse_term () =
    let parts = ref [] in
    let cont = ref true in
    while !cont do
      let c = peek () in
      if c = '\000' || c = ')' || c = '|' then cont := false
      else parts := parse_factor () :: !parts
    done;
    match List.rev !parts with
    | [single] -> single
    | l -> Concat l
  and parse_factor () =
    let node = ref (parse_atom ()) in
    while peek () = '*' do
      ignore (eat ());
      node := Star !node
    done;
    !node
  and parse_atom () =
    let c = eat () in
    if c = 'a' then Char 1
    else if c = 'b' then Char 2
    else if c = '(' then begin
      let node = parse_expr () in
      ignore (eat ());
      node
    end else failwith "unexpected char"
  in
  parse_expr ()

type nfa = {
  mutable adj : (int * int) list array;
  mutable num_states : int;
}

let make_nfa () = {
  adj = Array.make 256 [];
  num_states = 0;
}

let new_state nfa =
  if nfa.num_states >= Array.length nfa.adj then begin
    let new_adj = Array.make (2 * Array.length nfa.adj) [] in
    Array.blit nfa.adj 0 new_adj 0 nfa.num_states;
    nfa.adj <- new_adj
  end;
  let s = nfa.num_states in
  nfa.num_states <- nfa.num_states + 1;
  nfa.adj.(s) <- [];
  s

let add_edge nfa u v lab =
  nfa.adj.(u) <- (v, lab) :: nfa.adj.(u)

let rec build_nfa ast nfa =
  let start_s = new_state nfa in
  let end_s = new_state nfa in
  match ast with
  | Char c ->
    add_edge nfa start_s end_s c;
    (start_s, end_s)
  | Concat [] ->
    add_edge nfa start_s end_s 0;
    (start_s, end_s)
  | Concat kids ->
    let prev = ref (-1) in
    List.iter (fun k ->
      let (si, so) = build_nfa k nfa in
      if !prev = -1 then add_edge nfa start_s si 0
      else add_edge nfa !prev si 0;
      prev := so
    ) kids;
    add_edge nfa !prev end_s 0;
    (start_s, end_s)
  | Union (left, right) ->
    let (li, lo) = build_nfa left nfa in
    let (ri, ro) = build_nfa right nfa in
    add_edge nfa start_s li 0;
    add_edge nfa start_s ri 0;
    add_edge nfa lo end_s 0;
    add_edge nfa ro end_s 0;
    (start_s, end_s)
  | Star inner ->
    let (ii, io) = build_nfa inner nfa in
    add_edge nfa start_s ii 0;
    add_edge nfa start_s end_s 0;
    add_edge nfa io ii 0;
    add_edge nfa io end_s 0;
    (start_s, end_s)

let eclosure nfa starts =
  let seen = Array.make nfa.num_states false in
  let q = Queue.create () in
  List.iter (fun s ->
    if not seen.(s) then begin
      seen.(s) <- true;
      Queue.push s q
    end
  ) starts;
  while not (Queue.is_empty q) do
    let u = Queue.pop q in
    List.iter (fun (v, lab) ->
      if lab = 0 && not seen.(v) then begin
        seen.(v) <- true;
        Queue.push v q
      end
    ) nfa.adj.(u)
  done;
  let res = ref [] in
  for i = 0 to nfa.num_states - 1 do
    if seen.(i) then res := i :: !res
  done;
  List.rev !res

let move_set nfa states lab =
  let res = ref [] in
  List.iter (fun u ->
    List.iter (fun (v, elab) ->
      if elab = lab then res := v :: !res
    ) nfa.adj.(u)
  ) states;
  !res

let mat_mul a b =
  let n = Array.length a in
  let r = Array.make_matrix n n 0L in
  for i = 0 to n - 1 do
    for k = 0 to n - 1 do
      let aik = a.(i).(k) in
      if aik <> 0L then begin
        for j = 0 to n - 1 do
          let bkj = b.(k).(j) in
          if bkj <> 0L then
            r.(i).(j) <- Int64.rem (Int64.add r.(i).(j) (Int64.mul aik bkj)) mo
        done
      end
    done
  done;
  r

let mat_pow m p =
  let n = Array.length m in
  let r = Array.make_matrix n n 0L in
  for i = 0 to n - 1 do r.(i).(i) <- 1L done;
  let base = ref m in
  let p = ref p in
  let res = ref r in
  while !p > 0 do
    if !p land 1 = 1 then
      res := mat_mul !res !base;
    p := !p lsr 1;
    if !p > 0 then
      base := mat_mul !base !base
  done;
  !res

let count_strings regex l =
  let ast = parse_regex regex in
  let nfa = make_nfa () in
  let (start_nfa, end_nfa) = build_nfa ast nfa in
  let start_set = eclosure nfa [start_nfa] in
  let state_map = Hashtbl.create 64 in
  Hashtbl.add state_map start_set 0;
  let states = ref [| start_set |] in
  let dfa_adj = ref [| Array.make 2 0 |] in
  let q = Queue.create () in
  Queue.push 0 q;
  while not (Queue.is_empty q) do
    let cur = Queue.pop q in
    let cur_set = !states.(cur) in
    for lab_idx = 0 to 1 do
      let lab = lab_idx + 1 in
      let moved = move_set nfa cur_set lab in
      let to_set = if moved = [] then [] else eclosure nfa moved in
      let nxt =
        try Hashtbl.find state_map to_set
        with Not_found ->
          let idx = Array.length !states in
          Hashtbl.add state_map to_set idx;
          let new_states = Array.make (idx + 1) to_set in
          Array.blit !states 0 new_states 0 idx;
          states := new_states;
          let new_dfa = Array.make (idx + 1) (Array.make 2 0) in
          Array.blit !dfa_adj 0 new_dfa 0 idx;
          new_dfa.(idx) <- Array.make 2 0;
          dfa_adj := new_dfa;
          Queue.push idx q;
          idx
      in
      !dfa_adj.(cur).(lab_idx) <- nxt
    done
  done;
  let sz = Array.length !states in
  let end_states = ref [] in
  for i = 0 to sz - 1 do
    if List.mem end_nfa !states.(i) then end_states := i :: !end_states
  done;
  let mat = Array.make_matrix sz sz 0L in
  for i = 0 to sz - 1 do
    for c = 0 to 1 do
      let to_s = !dfa_adj.(i).(c) in
      mat.(i).(to_s) <- Int64.rem (Int64.add mat.(i).(to_s) 1L) mo
    done
  done;
  let mp = mat_pow mat l in
  let ans = ref 0L in
  List.iter (fun e ->
    ans := Int64.rem (Int64.add !ans mp.(0).(e)) mo
  ) !end_states;
  !ans

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      let r = Scanf.scanf " %s" (fun x -> x) in
      let l = Scanf.scanf " %d" (fun x -> x) in
      let ans = count_strings r l in
      Printf.printf "%Ld\n" ans
    done
  with End_of_file -> ()
