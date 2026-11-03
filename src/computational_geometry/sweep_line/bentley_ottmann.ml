type point = { x : float; y : float }
type segment = { id : int; p : point; q : point }

let eps = 1e-9

let cmp_pt a b =
  if abs_float (a.x -. b.x) > eps then compare a.x b.x
  else if abs_float (a.y -. b.y) > eps then compare a.y b.y
  else 0

let intersect s1 s2 =
  let x1 = s1.p.x in
  let y1 = s1.p.y in
  let x2 = s1.q.x in
  let y2 = s1.q.y in
  let x3 = s2.p.x in
  let y3 = s2.p.y in
  let x4 = s2.q.x in
  let y4 = s2.q.y in
  let denom = (x1 -. x2) *. (y3 -. y4) -. (y1 -. y2) *. (x3 -. x4) in
  if abs_float denom < eps then None
  else begin
    let t = ((x1 -. x3) *. (y3 -. y4) -. (y1 -. y3) *. (x3 -. x4)) /. denom in
    let u = -. ((x1 -. x2) *. (y1 -. y3) -. (y1 -. y2) *. (x1 -. x3)) /. denom in
    if t >= -.eps && t <= 1.0 +. eps && u >= -.eps && u <= 1.0 +. eps then
      Some { x = x1 +. t *. (x2 -. x1); y = y1 +. t *. (y2 -. y1) }
    else None
  end

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
    let segs = Array.init n (fun i ->
      let x1 = next_float () in
      let y1 = next_float () in
      let x2 = next_float () in
      let y2 = next_float () in
      let p1 = { x = x1; y = y1 } in
      let p2 = { x = x2; y = y2 } in
      let (p, q) = if cmp_pt p2 p1 < 0 then (p2, p1) else (p1, p2) in
      { id = i; p; q }
    ) in

    let pts = ref [] in
    for i = 0 to n - 1 do
      for j = i + 1 to n - 1 do
        match intersect segs.(i) segs.(j) with
        | Some pt -> pts := pt :: !pts
        | None -> ()
      done
    done;

    let pts_arr = Array.of_list !pts in
    Array.sort cmp_pt pts_arr;

    let unique_pts = ref [] in
    for i = 0 to Array.length pts_arr - 1 do
      match !unique_pts with
      | prev :: _ when cmp_pt prev pts_arr.(i) = 0 -> ()
      | _ -> unique_pts := pts_arr.(i) :: !unique_pts
    done;
    let res = List.rev !unique_pts in

    Printf.printf "%d\n" (List.length res);
    List.iter (fun pt ->
      Printf.printf "%.2f %.2f\n" pt.x pt.y
    ) res
  end
