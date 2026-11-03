type point = { x : float; y : float }

let dist a b =
  let dx = a.x -. b.x in
  let dy = a.y -. b.y in
  hypot dx dy

let rec closest_pair_rec (pts_x : point array) l r =
  if r - l <= 3 then begin
    let min_d = ref 1e18 in
    for i = l to r - 1 do
      for j = i + 1 to r - 1 do
        let d = dist pts_x.(i) pts_x.(j) in
        if d < !min_d then min_d := d
      done
    done;
    !min_d
  end else begin
    let mid = l + (r - l) / 2 in
    let mid_x = pts_x.(mid).x in
    let d1 = closest_pair_rec pts_x l mid in
    let d2 = closest_pair_rec pts_x mid r in
    let d = ref (min d1 d2) in
    let strip_list = ref [] in
    for i = l to r - 1 do
      if abs_float (pts_x.(i).x -. mid_x) < !d then
        strip_list := pts_x.(i) :: !strip_list
    done;
    let strip = Array.of_list !strip_list in
    Array.sort (fun a b -> compare a.y b.y) strip;
    let m = Array.length strip in
    for i = 0 to m - 1 do
      let j = ref (i + 1) in
      while !j < m && strip.(!j).y -. strip.(i).y < !d do
        let cur_d = dist strip.(i) strip.(!j) in
        if cur_d < !d then d := cur_d;
        incr j
      done
    done;
    !d
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
    let pts = Array.init n (fun _ ->
      let x = next_float () in
      let y = next_float () in
      { x; y }
    ) in
    Array.sort (fun a b ->
      let c = compare a.x b.x in
      if c <> 0 then c else compare a.y b.y
    ) pts;
    let ans = if n <= 1 then 0.0 else closest_pair_rec pts 0 n in
    Printf.printf "%.6f\n" ans
  end
