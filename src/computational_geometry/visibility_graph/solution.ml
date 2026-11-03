(* Visibility graph shortest path *)

type pt = { x : int64; y : int64 }

let orient a b c =
  let x1 = Int64.sub b.x a.x in
  let y1 = Int64.sub b.y a.y in
  let x2 = Int64.sub c.x a.x in
  let y2 = Int64.sub c.y a.y in
  Int64.sub (Int64.mul x1 y2) (Int64.mul y1 x2)

let proper_cross a b c d =
  let o1 = orient a b c in
  let o2 = orient a b d in
  let o3 = orient c d a in
  let o4 = orient c d b in
  let sgn x = if x > 0L then 1 else if x < 0L then -1 else 0 in
  (sgn o1 * sgn o2 = -1) && (sgn o3 * sgn o4 = -1)

let on_open p a b =
  if orient a b p <> 0L then false
  else
    let dot =
      Int64.add
        (Int64.mul (Int64.sub p.x a.x) (Int64.sub p.x b.x))
        (Int64.mul (Int64.sub p.y a.y) (Int64.sub p.y b.y))
    in
    dot < 0L

let on_closed2 px py a b =
  let ax = Int64.mul 2L a.x in
  let ay = Int64.mul 2L a.y in
  let bx = Int64.mul 2L b.x in
  let by = Int64.mul 2L b.y in
  let cross =
    Int64.sub
      (Int64.mul (Int64.sub bx ax) (Int64.sub py ay))
      (Int64.mul (Int64.sub by ay) (Int64.sub px ax))
  in
  if cross <> 0L then false
  else
    min ax bx <= px && px <= max ax bx &&
    min ay by <= py && py <= max ay by

let pip_strict x y poly =
  let m = Array.length poly in
  let on_boundary = ref false in
  for i = 0 to m - 1 do
    let next_i = (i + 1) mod m in
    if on_closed2 x y poly.(i) poly.(next_i) then
      on_boundary := true
  done;
  if !on_boundary then false
  else begin
    let inside = ref false in
    for i = 0 to m - 1 do
      let next_i = (i + 1) mod m in
      let cx = Int64.mul 2L poly.(i).x in
      let cy = Int64.mul 2L poly.(i).y in
      let dx = Int64.mul 2L poly.(next_i).x in
      let dy = Int64.mul 2L poly.(next_i).y in
      if (cy > y) <> (dy > y) then begin
        let cond =
          if dy > cy then
            Int64.mul x (Int64.sub dy cy) <
            Int64.add (Int64.mul cx (Int64.sub dy cy)) (Int64.mul (Int64.sub dx cx) (Int64.sub y cy))
          else
            Int64.mul x (Int64.sub dy cy) >
            Int64.add (Int64.mul cx (Int64.sub dy cy)) (Int64.mul (Int64.sub dx cx) (Int64.sub y cy))
        in
        if cond then inside := not !inside
      end
    done;
    !inside
  end

let visible a b obs =
  let ok = ref true in
  let oi = ref 0 in
  let num_obs = Array.length obs in
  while !oi < num_obs && !ok do
    let p = obs.(!oi) in
    let m = Array.length p in
    for i = 0 to m - 1 do
      let next_i = (i + 1) mod m in
      if proper_cross a b p.(i) p.(next_i) then
        ok := false
    done;
    if !ok then begin
      for i = 0 to m - 1 do
        let v = p.(i) in
        if on_open v a b then begin
          if pip_strict (Int64.add a.x v.x) (Int64.add a.y v.y) p then
            ok := false;
          if pip_strict (Int64.add v.x b.x) (Int64.add v.y b.y) p then
            ok := false
        end
      done
    end;
    incr oi
  done;
  !ok

let ew a b =
  let dx = Int64.to_float (Int64.sub a.x b.x) in
  let dy = Int64.to_float (Int64.sub a.y b.y) in
  sqrt (dx *. dx +. dy *. dy)

let () =
  let buf = Buffer.create 4096 in
  let b = Bytes.create 4096 in
  let rec read_all () =
    let n = input stdin b 0 4096 in
    if n > 0 then begin
      Buffer.add_subbytes buf b 0 n;
      read_all ()
    end
  in
  (try read_all () with End_of_file -> ());
  let content = Buffer.contents buf in
  let tokens =
    let len = String.length content in
    let rec collect acc i =
      if i >= len then List.rev acc
      else if content.[i] <= ' ' then collect acc (i + 1)
      else
        let j = ref i in
        while !j < len && content.[!j] > ' ' do incr j done;
        collect (String.sub content i (!j - i) :: acc) !j
    in
    Array.of_list (collect [] 0)
  in
  let pos = ref 0 in
  let next_token () =
    if !pos < Array.length tokens then
      let t = tokens.(!pos) in
      incr pos;
      t
    else raise End_of_file
  in
  try
    let n_str = next_token () in
    let n = int_of_string n_str in
    let obs = Array.init n (fun _ ->
      let k = int_of_string (next_token ()) in
      Array.init k (fun _ ->
        let x = Int64.of_string (next_token ()) in
        let y = Int64.of_string (next_token ()) in
        { x; y }
      )
    ) in
    let sx = Int64.of_string (next_token ()) in
    let sy = Int64.of_string (next_token ()) in
    let tx = Int64.of_string (next_token ()) in
    let ty = Int64.of_string (next_token ()) in
    let s = { x = sx; y = sy } in
    let t = { x = tx; y = ty } in

    let pts_list = ref [s; t] in
    Array.iter (fun p ->
      Array.iter (fun v -> pts_list := v :: !pts_list) p
    ) obs;
    (* Note: order in C++ was: s, t, then all polygon vertices in input order *)
    let other_pts = ref [] in
    Array.iter (fun p ->
      Array.iter (fun v -> other_pts := v :: !other_pts) p
    ) obs;
    let pts = Array.of_list (s :: t :: List.rev !other_pts) in
    let v_count = Array.length pts in

    let edges = ref [] in
    let w = Array.make_matrix v_count v_count (-1.0) in
    for i = 0 to v_count - 1 do
      for j = i + 1 to v_count - 1 do
        if visible pts.(i) pts.(j) obs then begin
          edges := (i, j) :: !edges;
          let d = ew pts.(i) pts.(j) in
          w.(i).(j) <- d;
          w.(j).(i) <- d
        end
      done
    done;

    let edge_arr = Array.of_list (List.rev !edges) in
    Printf.printf "%d\n" (Array.length edge_arr);
    Array.iter (fun (u, v) -> Printf.printf "%d %d\n" u v) edge_arr;

    let dist = Array.make v_count infinity in
    let prev = Array.make v_count (-1) in
    let used = Array.make v_count false in
    dist.(0) <- 0.0;

    for _ = 0 to v_count - 1 do
      let u = ref (-1) in
      for i = 0 to v_count - 1 do
        if not used.(i) && (!u = -1 || dist.(i) < dist.(!u)) then
          u := i
      done;
      if !u = -1 || dist.(!u) = infinity then ()
      else begin
        used.(!u) <- true;
        if !u = 1 then ()
        else begin
          for v = 0 to v_count - 1 do
            if w.(!u).(v) >= 0.0 && not used.(v) then begin
              let nd = dist.(!u) +. w.(!u).(v) in
              if nd < dist.(v) then begin
                dist.(v) <- nd;
                prev.(v) <- !u
              end
            end
          done
        end
      end
    done;

    if dist.(1) = infinity then begin
      Printf.printf "0\n\n-1\n"
    end else begin
      let path = ref [] in
      let curr = ref 1 in
      while !curr <> -1 do
        path := !curr :: !path;
        curr := prev.(!curr)
      done;
      Printf.printf "%d\n" (List.length !path);
      let rec print_path = function
        | [] -> ()
        | [x] -> Printf.printf "%d\n" x
        | x :: rest -> Printf.printf "%d " x; print_path rest
      in
      print_path !path;
      Printf.printf "%.6f\n" dist.(1)
    end
  with End_of_file -> ()
