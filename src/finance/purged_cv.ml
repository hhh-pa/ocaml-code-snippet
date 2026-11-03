let () =
  let tokens =
    let buf = Buffer.create 4096 in
    (try while true do Buffer.add_channel buf stdin 4096 done with End_of_file -> ());
    let s = Buffer.contents buf in
    let len = String.length s in
    let res = ref [] in
    let i = ref 0 in
    while !i < len do
      while !i < len && (let c = s.[!i] in c = ' ' || c = '\t' || c = '\r' || c = '\n') do incr i done;
      if !i < len then begin
        let st = !i in
        while !i < len && (let c = s.[!i] in c <> ' ' && c <> '\t' && c <> '\r' && c <> '\n') do incr i done;
        res := String.sub s st (!i - st) :: !res
      end
    done;
    Array.of_list (List.rev !res)
  in
  if Array.length tokens > 0 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let mode = next_str () in
    let p, k, e =
      if mode = "purged" then
        let p_val = next_int () in
        let e_val = next_int () in
        (p_val, 1, e_val)
      else
        let p_val = next_int () in
        let k_val = next_int () in
        let e_val = next_int () in
        (p_val, k_val, e_val)
    in
    let l = next_int () in
    let t0 = Array.make l 0 in
    let t1 = Array.make l 0 in
    for i = 0 to l - 1 do
      t0.(i) <- next_int ();
      t1.(i) <- next_int ()
    done;
    let parts = Array.init p (fun _ -> ref []) in
    for i = 0 to l - 1 do
      let part_idx = (i * p) / l in
      parts.(part_idx) := i :: !(parts.(part_idx))
    done;
    let parts_arr = Array.init p (fun i -> Array.of_list (List.rev !(parts.(i)))) in
    let tests = ref [] in
    if mode = "purged" then begin
      for f = 0 to p - 1 do
        tests := parts_arr.(f) :: !tests
      done;
      tests := List.rev !tests
    end else begin
      let comb = Array.init k (fun i -> i) in
      let rec gen () =
        let t_acc = ref [] in
        for i = 0 to k - 1 do
          let c = comb.(i) in
          let part = parts_arr.(c) in
          for j = 0 to Array.length part - 1 do
            t_acc := part.(j) :: !t_acc
          done
        done;
        tests := Array.of_list (List.rev !t_acc) :: !tests;
        let idx = ref (k - 1) in
        while !idx >= 0 && comb.(!idx) = p - k + !idx do
          decr idx
        done;
        if !idx >= 0 then begin
          comb.(!idx) <- comb.(!idx) + 1;
          for j = !idx + 1 to k - 1 do
            comb.(j) <- comb.(j - 1) + 1
          done;
          gen ()
        end
      in
      gen ();
      tests := List.rev !tests
    end;
    let tag = if mode = "purged" then "FOLD " else "SPLIT " in
    let tests_arr = Array.of_list !tests in
    for s = 0 to Array.length tests_arr - 1 do
      let test = tests_arr.(s) in
      let is_test = Array.make l false in
      for i = 0 to Array.length test - 1 do
        is_test.(test.(i)) <- true
      done;
      let train = ref [] in
      for i = 0 to l - 1 do
        if not is_test.(i) then train := i :: !train
      done;
      let train = List.rev !train in
      let tmin = ref (if Array.length test > 0 then t0.(test.(0)) else 0) in
      let tmax = ref (if Array.length test > 0 then t1.(test.(0)) else 0) in
      for i = 0 to Array.length test - 1 do
        let x = test.(i) in
        if t0.(x) < !tmin then tmin := t0.(x);
        if t1.(x) > !tmax then tmax := t1.(x)
      done;
      let kept = ref [] in
      List.iter (fun j ->
        let overlap = not (t1.(j) < !tmin || t0.(j) > !tmax) in
        if not overlap then begin
          let embargo = t0.(j) > !tmax && t0.(j) <= !tmax + e in
          if not embargo then kept := j :: !kept
        end
      ) train;
      let kept = List.rev !kept in
      let train_str =
        match kept with
        | [] -> "-"
        | _ -> String.concat "," (List.map string_of_int kept)
      in
      let test_str =
        if Array.length test = 0 then "-"
        else String.concat "," (List.map string_of_int (Array.to_list test))
      in
      Printf.printf "%s%d: train=%s test=%s\n" tag s train_str test_str
    done
  end
