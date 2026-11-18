(* LALR(1) table construction + parsing (Louden 5.4.3). *)

module SSet = Set.Make (String)
module SMap = Map.Make (String)

let read_all () =
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_char buf (input_char stdin)
     done
   with End_of_file -> ());
  Buffer.contents buf

let split_ws s =
  let toks = ref [] in
  let cur = Buffer.create 16 in
  let flush () =
    if Buffer.length cur > 0 then begin
      toks := Buffer.contents cur :: !toks;
      Buffer.clear cur
    end
  in
  String.iter
    (fun c ->
      match c with
      | ' ' | '\t' | '\n' | '\r' -> flush ()
      | _ -> Buffer.add_char cur c)
    s;
  flush ();
  List.rev !toks

let ikey p d la = Printf.sprintf "%d,%d,%s" p d la

let iparse key =
  let c1 = String.index key ',' in
  let c2 = String.index_from key (c1 + 1) ',' in
  ( int_of_string (String.sub key 0 c1),
    int_of_string (String.sub key (c1 + 1) (c2 - c1 - 1)),
    String.sub key (c2 + 1) (String.length key - c2 - 1) )

type prod = { lhs : string; rhs : string list }

let first_add first lhs rhs =
  if rhs = [] then
    if SSet.mem "" (SMap.find lhs first) then (first, false)
    else (SMap.add lhs (SSet.add "" (SMap.find lhs first)) first, true)
  else
    let changed = ref false in
    let first = ref first in
    let stop = ref false in
    let rest = ref rhs in
    while !rest <> [] && not !stop do
      let s = List.hd !rest in
      rest := List.tl !rest;
      if not (SMap.mem s !first) then (
        if not (SSet.mem s (SMap.find lhs !first)) then begin
          first := SMap.add lhs (SSet.add s (SMap.find lhs !first)) !first;
          changed := true
        end;
        stop := true)
      else
        let before = SSet.cardinal (SMap.find lhs !first) in
        first :=
          SMap.add lhs
            (SSet.union (SMap.find lhs !first)
               (SSet.remove "" (SMap.find s !first)))
            !first;
        if SSet.cardinal (SMap.find lhs !first) <> before then changed := true;
        if not (SSet.mem "" (SMap.find s !first)) then stop := true
        else if !rest = [] then
          if not (SSet.mem "" (SMap.find lhs !first)) then begin
            first := SMap.add lhs (SSet.add "" (SMap.find lhs !first)) !first;
            changed := true
          end
    done;
    (!first, !changed)

let first_sets prods =
  let first = ref SMap.empty in
  List.iter (fun pr -> first := SMap.add pr.lhs SSet.empty !first) prods;
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun pr ->
        let f, ch = first_add !first pr.lhs pr.rhs in
        first := f;
        changed := !changed || ch)
      prods
  done;
  !first

let first_seq first seq =
  let out = ref SSet.empty in
  let stop = ref false in
  let rest = ref seq in
  while !rest <> [] && not !stop do
    let s = List.hd !rest in
    rest := List.tl !rest;
    if not (SMap.mem s first) then begin
      out := SSet.add s !out;
      stop := true
    end
    else begin
      out := SSet.union !out (SSet.remove "" (SMap.find s first));
      if not (SSet.mem "" (SMap.find s first)) then stop := true
    end
  done;
  !out

let closure prods first items =
  let out = ref items in
  let work = ref (SSet.elements items) in
  while !work <> [] do
    let cur = List.hd !work in
    work := List.tl !work;
    let p, d, la = iparse cur in
    let rhs = (List.nth prods p).rhs in
    if d < List.length rhs && SMap.mem (List.nth rhs d) first then
      let b = List.nth rhs d in
      let beta = List.filteri (fun i _ -> i > d) rhs |> fun l -> l @ [ la ] in
      let fset = first_seq first beta in
      List.iteri
        (fun q pr ->
          if pr.lhs = b then
            SSet.iter
              (fun x ->
                let k = ikey q 0 x in
                if not (SSet.mem k !out) then begin
                  out := SSet.add k !out;
                  work := k :: !work
                end)
              fset)
        prods
  done;
  !out

let goto_state prods first items sym =
  let moved = ref SSet.empty in
  SSet.iter
    (fun it ->
      let p, d, la = iparse it in
      let rhs = (List.nth prods p).rhs in
      if d < List.length rhs && List.nth rhs d = sym then
        moved := SSet.add (ikey p (d + 1) la) !moved)
    items;
  if SSet.is_empty !moved then None else Some (closure prods first !moved)

let build_collection aug first terms nonterms =
  let start = closure aug first (SSet.singleton (ikey 0 0 "$")) in
  let states = ref [ start ] in
  let index = Hashtbl.create 16 in
  Hashtbl.replace index start 0;
  let trans = Hashtbl.create 16 in
  let queue = Queue.create () in
  Queue.push 0 queue;
  let syms = terms @ nonterms in
  while not (Queue.is_empty queue) do
    let si = Queue.pop queue in
    List.iter
      (fun x ->
        match goto_state aug first (List.nth !states si) x with
        | None -> ()
        | Some g ->
            let tgt =
              match Hashtbl.find_opt index g with
              | Some t -> t
              | None ->
                  let t = List.length !states in
                  Hashtbl.replace index g t;
                  states := !states @ [ g ];
                  Queue.push t queue;
                  t
            in
            Hashtbl.replace trans (si, x) tgt)
      syms
  done;
  (!states, trans)

let core_key st =
  SSet.elements st
  |> List.map (fun it ->
      let p, d, _ = iparse it in
      Printf.sprintf "%d,%d" p d)
  |> List.sort_uniq String.compare
  |> String.concat ";"

let find_target trans cls_of mem sym =
  let rec go = function
    | [] -> None
    | si :: rest -> (
        match Hashtbl.find_opt trans (si, sym) with
        | Some t -> Some (List.nth cls_of t)
        | None -> go rest)
  in
  go mem

let build_tables aug states trans terms nonterms =
  let term_set = List.fold_left (fun s x -> SSet.add x s) SSet.empty terms in
  let nonterm_set =
    List.fold_left (fun s x -> SSet.add x s) SSet.empty nonterms
  in
  let cores = ref [] in
  List.iter
    (fun st ->
      let k = core_key st in
      if not (List.mem k !cores) then cores := !cores @ [ k ])
    states;
  let cls_of =
    List.map
      (fun st ->
        let rec find i = function
          | [] -> failwith "nocore"
          | c :: _ when c = core_key st -> i
          | _ :: rest -> find (i + 1) rest
        in
        find 0 !cores)
      states
  in
  let members = Array.make (List.length !cores) [] in
  List.iteri (fun si ci -> members.(ci) <- members.(ci) @ [ si ]) cls_of;
  let action = Hashtbl.create 64 in
  let goto = Hashtbl.create 64 in
  let conflict = ref false in
  let cell key ent =
    (match Hashtbl.find_opt action key with
    | Some old when old <> ent -> conflict := true
    | _ -> ());
    Hashtbl.replace action key ent
  in
  Array.iteri
    (fun ci mem ->
      let look = Hashtbl.create 16 in
      List.iter
        (fun si ->
          SSet.iter
            (fun it ->
              let p, d, la = iparse it in
              let k = Printf.sprintf "%d,%d" p d in
              let prev =
                match Hashtbl.find_opt look k with
                | None -> SSet.empty
                | Some s -> s
              in
              Hashtbl.replace look k (SSet.add la prev))
            (List.nth states si))
        mem;
      Hashtbl.iter
        (fun k las ->
          let comma = String.index k ',' in
          let p = int_of_string (String.sub k 0 comma) in
          let d =
            int_of_string
              (String.sub k (comma + 1) (String.length k - comma - 1))
          in
          let rhs = (List.nth aug p).rhs in
          if d < List.length rhs && SSet.mem (List.nth rhs d) term_set then
            match find_target trans cls_of mem (List.nth rhs d) with
            | Some tgt -> cell (ci, List.nth rhs d) ('s', tgt)
            | None -> ()
          else if d = List.length rhs && p > 0 then
            SSet.iter (fun la -> cell (ci, la) ('r', p)) las
          else if d = List.length rhs && p = 0 then cell (ci, "$") ('a', 0);
          if d < List.length rhs && SSet.mem (List.nth rhs d) nonterm_set then
            match find_target trans cls_of mem (List.nth rhs d) with
            | Some tgt -> Hashtbl.replace goto (ci, List.nth rhs d) tgt
            | None -> ())
        look)
    members;
  (action, goto, Array.length members, !conflict, List.hd cls_of)

let parse_one aug action goto start qtoks =
  let toks = Array.of_list (qtoks @ [ "$" ]) in
  let rec loop stack pos reds =
    match Hashtbl.find_opt action (List.hd stack, toks.(pos)) with
    | None -> None
    | Some ('s', t) -> loop (t :: stack) (pos + 1) reds
    | Some ('r', p) -> (
        let rhs = (List.nth aug p).rhs in
        let rec drop n st = if n = 0 then st else drop (n - 1) (List.tl st) in
        let stack = drop (List.length rhs) stack in
        match Hashtbl.find_opt goto (List.hd stack, (List.nth aug p).lhs) with
        | None -> None
        | Some g -> loop (g :: stack) pos (reds @ [ p ]))
    | Some _ -> Some reds
  in
  loop [ start ] 0 []

let () =
  let text = read_all () in
  if split_ws text = [] then ()
  else
    let lines = String.split_on_char '\n' text in
    let m = int_of_string (List.hd lines) in
    let prods =
      List.init m (fun i ->
          match split_ws (List.nth lines (1 + i)) with
          | lhs :: _ :: rhs -> { lhs; rhs }
          | [ lhs ] -> { lhs; rhs = [] }
          | _ -> failwith "bad prod")
    in
    let q = int_of_string (List.nth lines (1 + m)) in
    let queries =
      List.init q (fun i ->
          let idx = 2 + m + i in
          if idx < List.length lines then split_ws (List.nth lines idx) else [])
    in
    let nonterms =
      List.fold_left
        (fun acc pr -> if List.mem pr.lhs acc then acc else acc @ [ pr.lhs ])
        [] prods
    in
    let terms =
      List.fold_left
        (fun acc pr ->
          List.fold_left
            (fun a s ->
              if List.mem s nonterms || List.mem s a then a else a @ [ s ])
            acc pr.rhs)
        [] prods
    in
    let first = first_sets prods in
    let aug = { lhs = "$S"; rhs = [ (List.hd prods).lhs ] } :: prods in
    let states, trans = build_collection aug first terms nonterms in
    let action, goto, ncls, conflict, start =
      build_tables aug states trans terms nonterms
    in
    if conflict then print_string "CONFLICT"
    else
      let out = Buffer.create 256 in
      Buffer.add_string out (Printf.sprintf "STATES %d\n" ncls);
      List.iter
        (fun qtoks ->
          match parse_one aug action goto start qtoks with
          | None -> Buffer.add_string out "REJECT\n"
          | Some [] -> Buffer.add_string out "ACCEPT\n"
          | Some reds ->
              Buffer.add_string out
                ("ACCEPT "
                ^ String.concat " " (List.map string_of_int reds)
                ^ "\n"))
        queries;
      print_string (Buffer.contents out)
