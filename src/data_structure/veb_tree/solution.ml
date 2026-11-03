type veb_tree = {
  u : int;
  lower_sqrt : int;
  upper_sqrt : int;
  mutable min_val : int;
  mutable max_val : int;
  mutable summary : veb_tree option;
  cluster : veb_tree option array;
}

let rec make_veb universe =
  let k = ref 0 in
  while (1 lsl !k) < universe do
    incr k
  done;
  let lower_k = !k / 2 in
  let upper_k = !k - lower_k in
  let lower_sqrt = 1 lsl lower_k in
  let upper_sqrt = 1 lsl upper_k in
  let cluster = if universe > 2 then Array.make upper_sqrt None else [||] in
  {
    u = universe;
    lower_sqrt;
    upper_sqrt;
    min_val = -1;
    max_val = -1;
    summary = None;
    cluster;
  }

let high t x = x / t.lower_sqrt
let low t x = x mod t.lower_sqrt
let index t h l = h * t.lower_sqrt + l

let rec member t x =
  if x < 0 || x >= t.u then false
  else if x = t.min_val || x = t.max_val then true
  else if t.u = 2 then false
  else
    let h = high t x in
    match t.cluster.(h) with
    | None -> false
    | Some c -> member c (low t x)

let rec insert t x_in =
  let x = ref x_in in
  if !x >= 0 && !x < t.u then begin
    if t.min_val = -1 then begin
      t.min_val <- !x;
      t.max_val <- !x
    end else if !x <> t.min_val && !x <> t.max_val then begin
      if !x < t.min_val then begin
        let tmp = !x in
        x := t.min_val;
        t.min_val <- tmp
      end;
      if t.u > 2 then begin
        let h = high t !x in
        let l = low t !x in
        let c =
          match t.cluster.(h) with
          | Some c -> c
          | None ->
            let new_c = make_veb t.lower_sqrt in
            t.cluster.(h) <- Some new_c;
            new_c
        in
        if c.min_val = -1 then begin
          let summ =
            match t.summary with
            | Some s -> s
            | None ->
              let new_s = make_veb t.upper_sqrt in
              t.summary <- Some new_s;
              new_s
          in
          insert summ h;
          c.min_val <- l;
          c.max_val <- l
        end else
          insert c l
      end;
      if !x > t.max_val then t.max_val <- !x
    end
  end

let rec remove_present t x_in =
  let x = ref x_in in
  if t.min_val = t.max_val then begin
    t.min_val <- -1;
    t.max_val <- -1
  end else if t.u = 2 then begin
    t.min_val <- (if !x = 0 then 1 else 0);
    t.max_val <- t.min_val
  end else begin
    if !x = t.min_val then begin
      let summ = match t.summary with Some s -> s | None -> assert false in
      let first_cluster = summ.min_val in
      let c = match t.cluster.(first_cluster) with Some c -> c | None -> assert false in
      x := index t first_cluster c.min_val;
      t.min_val <- !x
    end;
    let h = high t !x in
    let l = low t !x in
    let c = match t.cluster.(h) with Some c -> c | None -> assert false in
    remove_present c l;
    if c.min_val = -1 then begin
      let summ = match t.summary with Some s -> s | None -> assert false in
      remove_present summ h;
      if !x = t.max_val then begin
        let summary_max = summ.max_val in
        if summary_max = -1 then
          t.max_val <- t.min_val
        else
          let c_max = match t.cluster.(summary_max) with Some c -> c | None -> assert false in
          t.max_val <- index t summary_max c_max.max_val
      end
    end else if !x = t.max_val then
      t.max_val <- index t h c.max_val
  end

let remove t x =
  if member t x then remove_present t x

let rec successor t x =
  if t.min_val = -1 || x >= t.max_val then -1
  else if x < t.min_val then t.min_val
  else if t.u = 2 then
    if x = 0 && t.max_val = 1 then 1 else -1
  else
    let h = high t x in
    let l = low t x in
    let max_low =
      match t.cluster.(h) with
      | Some c -> c.max_val
      | None -> -1
    in
    if max_low <> -1 && l < max_low then
      let c = match t.cluster.(h) with Some c -> c | None -> assert false in
      let offset = successor c l in
      index t h offset
    else
      let succ_cluster =
        match t.summary with
        | Some s -> successor s h
        | None -> -1
      in
      if succ_cluster = -1 then -1
      else
        let c = match t.cluster.(succ_cluster) with Some c -> c | None -> assert false in
        let offset = c.min_val in
        index t succ_cluster offset

let rec predecessor t x =
  if t.min_val = -1 || x <= t.min_val then -1
  else if x > t.max_val then t.max_val
  else if t.u = 2 then
    if x = 1 && t.min_val = 0 then 0 else -1
  else
    let h = high t x in
    let l = low t x in
    let min_low =
      match t.cluster.(h) with
      | Some c -> c.min_val
      | None -> -1
    in
    if min_low <> -1 && l > min_low then
      let c = match t.cluster.(h) with Some c -> c | None -> assert false in
      let offset = predecessor c l in
      index t h offset
    else
      let pred_cluster =
        match t.summary with
        | Some s -> predecessor s h
        | None -> -1
      in
      if pred_cluster = -1 then
        if t.min_val <> -1 && x > t.min_val then t.min_val else -1
      else
        let c = match t.cluster.(pred_cluster) with Some c -> c | None -> assert false in
        let offset = c.max_val in
        index t pred_cluster offset

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
  if Array.length tokens >= 2 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let u = next_int () in
    let q = next_int () in
    let tree = make_veb u in
    for _ = 0 to q - 1 do
      if !pos < Array.length tokens then begin
        let op = next_int () in
        if op = 1 then begin
          let x = next_int () in
          insert tree x
        end else if op = 2 then begin
          let x = next_int () in
          remove tree x
        end else if op = 3 then begin
          let x = next_int () in
          Printf.printf "%d\n" (if member tree x then 1 else 0)
        end else if op = 4 then begin
          let x = next_int () in
          Printf.printf "%d\n" (successor tree x)
        end else if op = 5 then begin
          let x = next_int () in
          Printf.printf "%d\n" (predecessor tree x)
        end else if op = 6 then begin
          Printf.printf "%d %d\n" tree.min_val tree.max_val
        end
      end
    done
  end
