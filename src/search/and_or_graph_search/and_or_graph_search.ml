module IntSet = Set.Make(Int)

let solve_case () =
  let n = Scanf.scanf " %d" (fun x -> x) in
  let typ = Array.make n 0 in
  for i = 0 to n - 1 do
    typ.(i) <- Scanf.scanf " %d" (fun x -> x)
  done;
  let children = Array.make n [] in
  for i = 0 to n - 1 do
    if typ.(i) = 0 || typ.(i) = 1 then begin
      let k = Scanf.scanf " %d" (fun x -> x) in
      let c = ref [] in
      for _ = 0 to k - 1 do
        c := Scanf.scanf " %d" (fun x -> x) :: !c
      done;
      children.(i) <- List.rev !c
    end
  done;
  let memo = Array.make n (-1) in
  let memo_plan = Array.make n IntSet.empty in
  let rec solve node =
    if memo.(node) <> -1 then begin
      if memo.(node) = 0 then (false, IntSet.empty)
      else (true, memo_plan.(node))
    end else if typ.(node) = 2 then begin
      let s = IntSet.singleton node in
      memo.(node) <- 1;
      memo_plan.(node) <- s;
      (true, s)
    end else if typ.(node) = 3 then begin
      memo.(node) <- 0;
      (false, IntSet.empty)
    end else if typ.(node) = 1 then begin
      let combined = ref (IntSet.singleton node) in
      let all_ok = ref true in
      let rec loop = function
        | [] -> ()
        | c :: rest ->
            let (ok, sub) = solve c in
            if not ok then all_ok := false
            else begin
              combined := IntSet.union !combined sub;
              loop rest
            end
      in
      loop children.(node);
      if not !all_ok then begin
        memo.(node) <- 0;
        (false, IntSet.empty)
      end else begin
        memo.(node) <- 1;
        memo_plan.(node) <- !combined;
        (true, !combined)
      end
    end else begin
      (* OR node *)
      let found = ref None in
      let rec loop = function
        | [] -> ()
        | c :: rest ->
            let (ok, sub) = solve c in
            if ok then found := Some sub
            else loop rest
      in
      loop children.(node);
      match !found with
      | Some sub ->
          let combined = IntSet.add node sub in
          memo.(node) <- 1;
          memo_plan.(node) <- combined;
          (true, combined)
      | None ->
          memo.(node) <- 0;
          (false, IntSet.empty)
    end
  in
  let (ok, plan) = solve 0 in
  if not ok then
    Printf.printf "UNSOLVABLE\n"
  else begin
    let elements = IntSet.elements plan in
    Printf.printf "SOLVABLE %d\n" (List.length elements);
    let first = ref true in
    List.iter (fun x ->
      if not !first then Printf.printf " ";
      Printf.printf "%d" x;
      first := false
    ) elements;
    Printf.printf "\n"
  end

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
