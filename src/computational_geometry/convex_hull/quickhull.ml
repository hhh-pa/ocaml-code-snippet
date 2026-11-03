type point = { x : int64; y : int64 }

let cross o a b =
  let dx1 = Int64.sub a.x o.x in
  let dy1 = Int64.sub b.y o.y in
  let dy2 = Int64.sub a.y o.y in
  let dx2 = Int64.sub b.x o.x in
  Int64.sub (Int64.mul dx1 dy1) (Int64.mul dy2 dx2)

let cmp a b =
  let c = Int64.compare a.x b.x in
  if c <> 0 then c else Int64.compare a.y b.y

let rec build_hull p q pts out =
  if pts <> [] then begin
    let pts_arr = Array.of_list pts in
    let best_idx = ref 0 in
    let best_dist = ref (cross q p pts_arr.(0)) in
    for i = 1 to Array.length pts_arr - 1 do
      let d = cross q p pts_arr.(i) in
      if d > !best_dist || (d = !best_dist && cmp pts_arr.(i) pts_arr.(!best_idx) < 0) then begin
        best_dist := d;
        best_idx := i
      end
    done;
    let c = pts_arr.(!best_idx) in
    let left_pc = ref [] in
    let left_cq = ref [] in
    List.iter (fun pt ->
      if cross c p pt > 0L then left_pc := pt :: !left_pc
      else if cross q c pt > 0L then left_cq := pt :: !left_cq
    ) pts;
    build_hull p c (List.rev !left_pc) out;
    out := c :: !out;
    build_hull c q (List.rev !left_cq) out
  end

let quickhull pts_arr =
  Array.sort cmp pts_arr;
  let pts = ref [] in
  for i = 0 to Array.length pts_arr - 1 do
    match !pts with
    | prev :: _ when prev.x = pts_arr.(i).x && prev.y = pts_arr.(i).y -> ()
    | _ -> pts := pts_arr.(i) :: !pts
  done;
  let pts = Array.of_list (List.rev !pts) in
  let n = Array.length pts in
  if n <= 2 then Array.to_list pts
  else begin
    let a = pts.(0) in
    let b = pts.(n - 1) in
    let lower_pts = ref [] in
    let upper_pts = ref [] in
    for i = 1 to n - 2 do
      let cp = cross a b pts.(i) in
      if cp < 0L then lower_pts := pts.(i) :: !lower_pts
      else if cp > 0L then upper_pts := pts.(i) :: !upper_pts
    done;
    let lower_pts = List.rev !lower_pts in
    let upper_pts = List.rev !upper_pts in
    if lower_pts = [] && upper_pts = [] then [a; b]
    else begin
      let raw = ref [a] in
      build_hull a b lower_pts raw;
      raw := b :: !raw;
      build_hull b a upper_pts raw;
      let raw_arr = Array.of_list (List.rev !raw) in
      let m = Array.length raw_arr in
      let hull = ref [] in
      for i = 0 to m - 1 do
        let prev = raw_arr.((i + m - 1) mod m) in
        let curr = raw_arr.(i) in
        let nxt = raw_arr.((i + 1) mod m) in
        if cross prev curr nxt > 0L then hull := curr :: !hull
      done;
      let hull_arr = Array.of_list (List.rev !hull) in
      let hlen = Array.length hull_arr in
      if hlen = 0 then []
      else begin
        let min_idx = ref 0 in
        for i = 1 to hlen - 1 do
          if cmp hull_arr.(i) hull_arr.(!min_idx) < 0 then min_idx := i
        done;
        let res = Array.init hlen (fun i -> hull_arr.((!min_idx + i) mod hlen)) in
        Array.to_list res
      end
    end
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
    let next_int64 () = Int64.of_string (next_str ()) in
    let n = next_int () in
    if n > 0 then begin
      let pts = Array.init n (fun _ ->
        let x = next_int64 () in
        let y = next_int64 () in
        { x; y }
      ) in
      let hull = quickhull pts in
      List.iter (fun p -> Printf.printf "%Ld %Ld\n" p.x p.y) hull
    end
  end
