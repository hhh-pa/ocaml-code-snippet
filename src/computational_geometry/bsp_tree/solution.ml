type seg = { x1 : int64; y1 : int64; x2 : int64; y2 : int64 }

type bsp_node = {
  sp : seg;
  at : int list;
  mutable left_child : int;
  mutable right_child : int;
}

let orient sp px py =
  let dx = Int64.sub sp.x2 sp.x1 in
  let dy = Int64.sub sp.y2 sp.y1 in
  Int64.sub
    (Int64.mul dx (Int64.sub py sp.y1))
    (Int64.mul dy (Int64.sub px sp.x1))

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
    let segs = Array.init n (fun _ ->
      let x1 = next_int64 () in
      let y1 = next_int64 () in
      let x2 = next_int64 () in
      let y2 = next_int64 () in
      { x1; y1; x2; y2 }
    ) in

    let nodes = ref [] in

    let rec build ids =
      match ids with
      | [] -> -1
      | first_id :: rest ->
        let sp = segs.(first_id) in
        let at = ref [first_id] in
        let left = ref [] in
        let right = ref [] in
        List.iter (fun i ->
          let s = segs.(i) in
          let o1 = orient sp s.x1 s.y1 in
          let o2 = orient sp s.x2 s.y2 in
          if o1 < 0L && o2 < 0L then left := i :: !left
          else if o1 > 0L && o2 > 0L then right := i :: !right
          else at := i :: !at
        ) rest;
        let left_ids = List.rev !left in
        let right_ids = List.rev !right in
        let at_ids = List.rev !at in
        let nd = {
          sp;
          at = at_ids;
          left_child = -1;
          right_child = -1;
        } in
        let idx = List.length !nodes in
        nodes := nd :: !nodes;
        let l_idx = build left_ids in
        let r_idx = build right_ids in
        nd.left_child <- l_idx;
        nd.right_child <- r_idx;
        idx
    in

    let all_ids = List.init n (fun i -> i) in
    ignore (build all_ids);
    let nodes_arr = Array.of_list (List.rev !nodes) in

    Printf.printf "%d\n" (Array.length nodes_arr);
    for i = 0 to Array.length nodes_arr - 1 do
      let nd = nodes_arr.(i) in
      Printf.printf "%Ld %Ld %Ld %Ld %d %d %d"
        nd.sp.x1 nd.sp.y1 nd.sp.x2 nd.sp.y2 nd.left_child nd.right_child (List.length nd.at);
      List.iter (fun at_id -> Printf.printf " %d" at_id) nd.at;
      print_newline ()
    done;

    if !pos < Array.length tokens then begin
      let q = next_int () in
      for _ = 0 to q - 1 do
        if !pos < Array.length tokens then begin
          let x = next_int64 () in
          let y = next_int64 () in
          let node = ref 0 in
          let path = ref [] in
          let stop = ref false in
          while not !stop do
            let nd = nodes_arr.(!node) in
            let o = orient nd.sp x y in
            if o < 0L then begin
              if nd.left_child = -1 then stop := true
              else begin
                path := "L" :: !path;
                node := nd.left_child
              end
            end else if o > 0L then begin
              if nd.right_child = -1 then stop := true
              else begin
                path := "R" :: !path;
                node := nd.right_child
              end
            end else begin
              path := "=" :: !path;
              stop := true
            end
          done;
          let p_list = List.rev !path in
          let p_str = if p_list = [] then "-" else String.concat " " p_list in
          Printf.printf "%s %d\n" p_str !node
        end
      done
    end
  end
