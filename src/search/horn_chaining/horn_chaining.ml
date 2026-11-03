let solve_case () =
  let v_cnt = Scanf.scanf " %d" (fun x -> x) in
  let r_cnt = Scanf.scanf " %d" (fun x -> x) in
  let prem = Array.make r_cnt [] in
  let concl = Array.make r_cnt 0 in
  let byprem = Array.make v_cnt [] in
  for i = 0 to r_cnt - 1 do
    let k = Scanf.scanf " %d" (fun x -> x) in
    let pl = ref [] in
    for _ = 0 to k - 1 do
      let x = Scanf.scanf " %d" (fun v -> v) in
      pl := x :: !pl;
      byprem.(x) <- i :: byprem.(x)
    done;
    prem.(i) <- List.rev !pl;
    concl.(i) <- Scanf.scanf " %d" (fun x -> x)
  done;
  for x = 0 to v_cnt - 1 do
    byprem.(x) <- List.rev byprem.(x)
  done;
  let q = Scanf.scanf " %d" (fun x -> x) in

  (* Forward chaining *)
  let count = Array.make r_cnt 0 in
  for i = 0 to r_cnt - 1 do
    count.(i) <- List.length prem.(i)
  done;
  let inferred = Array.make v_cnt false in
  let ag = Queue.create () in
  for i = 0 to r_cnt - 1 do
    if count.(i) = 0 && not inferred.(concl.(i)) then begin
      inferred.(concl.(i)) <- true;
      Queue.push concl.(i) ag
    end
  done;
  let fc = ref false in
  while not (Queue.is_empty ag) && not !fc do
    let p = Queue.pop ag in
    if p = q then fc := true
    else begin
      List.iter (fun ri ->
        count.(ri) <- count.(ri) - 1;
        if count.(ri) = 0 then begin
          let c = concl.(ri) in
          if not inferred.(c) then begin
            inferred.(c) <- true;
            Queue.push c ag
          end
        end
      ) byprem.(p)
    end
  done;

  (* Backward chaining *)
  let byconcl = Array.make v_cnt [] in
  for i = 0 to r_cnt - 1 do
    byconcl.(concl.(i)) <- i :: byconcl.(concl.(i))
  done;
  for x = 0 to v_cnt - 1 do
    byconcl.(x) <- List.rev byconcl.(x)
  done;

  let isfact = Array.make v_cnt false in
  for i = 0 to r_cnt - 1 do
    if prem.(i) = [] then isfact.(concl.(i)) <- true
  done;

  let proved = Array.make v_cnt false in
  let visiting = Array.make v_cnt false in

  let rec prove g =
    if proved.(g) then true
    else if visiting.(g) then false
    else if isfact.(g) then begin
      proved.(g) <- true;
      true
    end else begin
      visiting.(g) <- true;
      let rec try_rules = function
        | [] ->
            visiting.(g) <- false;
            false
        | ri :: rest ->
            let all_ok = List.for_all prove prem.(ri) in
            if all_ok then begin
              proved.(g) <- true;
              visiting.(g) <- false;
              true
            end else try_rules rest
      in
      try_rules byconcl.(g)
    end
  in
  let bc = prove q in
  Printf.printf "HORN FC:%s BC:%s\n" (if !fc then "YES" else "NO") (if bc then "YES" else "NO")

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
