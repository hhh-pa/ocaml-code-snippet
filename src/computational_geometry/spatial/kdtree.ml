type point = { x : float; y : float }

type kd_node = {
  pt : point;
  mutable left : int;
  mutable right : int;
  axis : int;
}

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

    let tree = ref [] in

    let rec build l r depth =
      if l >= r then -1
      else begin
        let axis = depth mod 2 in
        let sub_len = r - l in
        let sub = Array.sub pts l sub_len in
        if axis = 0 then
          Array.stable_sort (fun a b -> compare a.x b.x) sub
        else
          Array.stable_sort (fun a b -> compare a.y b.y) sub;
        Array.blit sub 0 pts l sub_len;

        let mid = l + sub_len / 2 in
        let node_idx = List.length !tree in
        let nd = { pt = pts.(mid); left = -1; right = -1; axis } in
        tree := nd :: !tree;

        let left_child = build l mid (depth + 1) in
        let right_child = build (mid + 1) r (depth + 1) in
        nd.left <- left_child;
        nd.right <- right_child;
        node_idx
      end
    in

    let root = if n > 0 then build 0 n 0 else -1 in
    let tree_arr = Array.of_list (List.rev !tree) in

    let rec range_query u xmin ymin xmax ymax =
      if u = -1 then 0
      else begin
        let node = tree_arr.(u) in
        let cur = if node.pt.x >= xmin && node.pt.x <= xmax && node.pt.y >= ymin && node.pt.y <= ymax then 1 else 0 in
        let left_cnt =
          if (node.axis = 0 && xmin <= node.pt.x) || (node.axis = 1 && ymin <= node.pt.y) then
            range_query node.left xmin ymin xmax ymax
          else 0
        in
        let right_cnt =
          if (node.axis = 0 && xmax >= node.pt.x) || (node.axis = 1 && ymax >= node.pt.y) then
            range_query node.right xmin ymin xmax ymax
          else 0
        in
        cur + left_cnt + right_cnt
      end
    in

    let best_dist_sq = ref 1e18 in
    let best_pt = ref { x = 0.0; y = 0.0 } in

    let rec nearest_neighbor u qx qy =
      if u <> -1 then begin
        let node = tree_arr.(u) in
        let dx = node.pt.x -. qx in
        let dy = node.pt.y -. qy in
        let d_sq = dx *. dx +. dy *. dy in
        if d_sq < !best_dist_sq then begin
          best_dist_sq := d_sq;
          best_pt := node.pt
        end;
        let axis_diff = if node.axis = 0 then qx -. node.pt.x else qy -. node.pt.y in
        let first = if axis_diff <= 0.0 then node.left else node.right in
        let second = if axis_diff <= 0.0 then node.right else node.left in
        nearest_neighbor first qx qy;
        if axis_diff *. axis_diff < !best_dist_sq then
          nearest_neighbor second qx qy
      end
    in

    if !pos < Array.length tokens then begin
      let num_queries = next_int () in
      for _ = 0 to num_queries - 1 do
        if !pos < Array.length tokens then begin
          let qtype = next_str () in
          if qtype = "RANGE" then begin
            let x1 = next_float () in
            let y1 = next_float () in
            let x2 = next_float () in
            let y2 = next_float () in
            Printf.printf "%d\n" (range_query root x1 y1 x2 y2)
          end else if qtype = "NN" then begin
            let qx = next_float () in
            let qy = next_float () in
            best_dist_sq := 1e18;
            best_pt := { x = 0.0; y = 0.0 };
            nearest_neighbor root qx qy;
            Printf.printf "%.2f %.2f %.2f\n" !best_pt.x !best_pt.y (sqrt !best_dist_sq)
          end
        end
      done
    end
  end
