type point = { x : float; y : float }

let dist p1 p2 =
  hypot (p1.x -. p2.x) (p1.y -. p2.y)

let perimeter p =
  let n = Array.length p in
  if n = 0 then 0.0
  else if n = 1 then 0.0
  else begin
    let ans = ref 0.0 in
    for i = 0 to n - 2 do
      ans := !ans +. dist p.(i) p.(i + 1)
    done;
    ans := !ans +. dist p.(0) p.(n - 1);
    !ans
  end

let cross o a b =
  (a.x -. o.x) *. (b.y -. o.y) -. (a.y -. o.y) *. (b.x -. o.x)

let hull_sub points =
  let ans = ref [] in
  List.iter (fun p ->
    let rec pop () =
      match !ans with
      | p2 :: p1 :: rest when cross p1 p2 p <= 0.0 ->
        ans := p1 :: rest;
        pop ()
      | _ -> ()
    in
    pop ();
    ans := p :: !ans
  ) points;
  match !ans with
  | [] -> []
  | _ :: rest -> List.rev rest

let cmp a b =
  let c = compare a.x b.x in
  if c <> 0 then c else compare a.y b.y

let convex_hull points =
  let pts_arr = Array.of_list points in
  Array.sort cmp pts_arr;
  let distinct = ref [] in
  for i = 0 to Array.length pts_arr - 1 do
    match !distinct with
    | prev :: _ when prev.x = pts_arr.(i).x && prev.y = pts_arr.(i).y -> ()
    | _ -> distinct := pts_arr.(i) :: !distinct
  done;
  let sorted_pts = List.rev !distinct in
  let num_distinct = List.length sorted_pts in
  if num_distinct > 2 then begin
    let lower = hull_sub sorted_pts in
    let upper = hull_sub (List.rev sorted_pts) in
    Array.of_list (lower @ upper)
  end else
    Array.of_list sorted_pts

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
  if Array.length tokens >= 1 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let next_float () = float_of_string (next_str ()) in
    let n = next_int () in
    let pts = ref [] in
    for _ = 0 to n - 1 do
      let x = next_float () in
      let y = next_float () in
      pts := { x; y } :: !pts
    done;
    let ch = convex_hull (List.rev !pts) in
    Printf.printf "%.1f\n" (perimeter ch)
  end
