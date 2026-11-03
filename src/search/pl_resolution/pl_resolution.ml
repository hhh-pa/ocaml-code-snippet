let norm_clause l =
  let sorted = List.sort compare l in
  let rec dedup = function
    | [] -> []
    | [x] -> [x]
    | x :: y :: rest ->
      if x = y then dedup (y :: rest)
      else x :: dedup (y :: rest)
  in
  dedup sorted

let is_taut c =
  List.exists (fun x -> List.mem (-x) c) c

let solve_case () =
  let _v = Scanf.scanf " %d" (fun x -> x) in
  let c_cnt = Scanf.scanf " %d" (fun x -> x) in
  let clauses = ref [] in
  let seen = Hashtbl.create 1024 in
  for _ = 0 to c_cnt - 1 do
    let k = Scanf.scanf " %d" (fun x -> x) in
    let c = ref [] in
    for _ = 0 to k - 1 do
      c := Scanf.scanf " %d" (fun x -> x) :: !c
    done;
    let nc = norm_clause !c in
    if not (is_taut nc) && not (Hashtbl.mem seen nc) then begin
      Hashtbl.add seen nc ();
      clauses := nc :: !clauses
    end
  done;
  let q = Scanf.scanf " %d" (fun x -> x) in
  let nq = [-q] in
  if not (Hashtbl.mem seen nq) then begin
    Hashtbl.add seen nq ();
    clauses := nq :: !clauses
  end;
  let clause_arr = ref (Array.of_list (List.rev !clauses)) in
  let entailed = ref false in
  let round = ref 0 in
  while !round < 500 && not !entailed do
    incr round;
    let added = ref [] in
    let added_tbl = Hashtbl.create 256 in
    let n = Array.length !clause_arr in
    let i = ref 0 in
    while !i < n && not !entailed do
      let ci = !clause_arr.(!i) in
      let j = ref (!i + 1) in
      while !j < n && not !entailed do
        let cj = !clause_arr.(!j) in
        List.iter (fun l1 ->
          if not !entailed && List.mem (-l1) cj then begin
            let r = List.filter (fun x -> x <> l1) ci @ List.filter (fun x -> x <> -l1) cj in
            let nr = norm_clause r in
            if nr = [] then entailed := true
            else if not (is_taut nr) then begin
              if not (Hashtbl.mem seen nr) && not (Hashtbl.mem added_tbl nr) then begin
                Hashtbl.add added_tbl nr ();
                added := nr :: !added
              end
            end
          end
        ) ci;
        incr j
      done;
      incr i
    done;
    if !added = [] then round := 500
    else begin
      List.iter (fun c -> Hashtbl.add seen c ()) !added;
      let new_arr = Array.append !clause_arr (Array.of_list (List.rev !added)) in
      clause_arr := new_arr;
      if Array.length !clause_arr > 4000 then round := 500
    end
  done;
  if !entailed then
    Printf.printf "RESOLUTION ENTAILED\n"
  else
    Printf.printf "RESOLUTION NOT-ENTAILED\n"

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
