type quad_node = {
  leaf : bool;
  x : int;
  y : int;
  s : int;
  c : int array;
  ids : int list;
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
  if Array.length tokens >= 3 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let n = next_int () in
    let big_s = next_int () in
    let cap = next_int () in
    let px = Array.make n 0 in
    let py = Array.make n 0 in
    for i = 0 to n - 1 do
      px.(i) <- next_int ();
      py.(i) <- next_int ()
    done;

    let nodes = ref [] in

    let rec rec_build ids x y s =
      if List.length ids <= cap || s = 1 then begin
        let nd = {
          leaf = true;
          x;
          y;
          s;
          c = [| -1; -1; -1; -1 |];
          ids;
        } in
        let idx = List.length !nodes in
        nodes := nd :: !nodes;
        idx
      end else begin
        let h = s / 2 in
        let q = Array.init 4 (fun _ -> ref []) in
        List.iter (fun i ->
          let k = (if px.(i) < x + h then 0 else 1) + (if py.(i) < y + h then 0 else 2) in
          q.(k) := i :: !(q.(k))
        ) ids;
        let q = Array.map (fun r -> List.rev !r) q in
        let nd = {
          leaf = false;
          x;
          y;
          s;
          c = [| -1; -1; -1; -1 |];
          ids = [];
        } in
        let idx = List.length !nodes in
        nodes := nd :: !nodes;
        let qx = [| x; x + h; x; x + h |] in
        let qy = [| y; y; y + h; y + h |] in
        let qs = [| h; s - h; h; s - h |] in
        for k = 0 to 3 do
          nd.c.(k) <- (if q.(k) = [] then -1 else rec_build q.(k) qx.(k) qy.(k) qs.(k))
        done;
        idx
      end
    in

    let all_ids = List.init n (fun i -> i) in
    ignore (rec_build all_ids 0 0 big_s);
    let nodes_arr = Array.of_list (List.rev !nodes) in

    Printf.printf "%d\n" (Array.length nodes_arr);
    for i = 0 to Array.length nodes_arr - 1 do
      let nd = nodes_arr.(i) in
      if not nd.leaf then
        Printf.printf "I %d %d %d %d %d %d %d\n" nd.x nd.y nd.s nd.c.(0) nd.c.(1) nd.c.(2) nd.c.(3)
      else begin
        Printf.printf "L %d %d %d %d" nd.x nd.y nd.s (List.length nd.ids);
        List.iter (fun id -> Printf.printf " %d" id) nd.ids;
        print_newline ()
      end
    done;

    let rec count_pts ni x1 y1 x2 y2 =
      let nd = nodes_arr.(ni) in
      if nd.leaf then begin
        let c = ref 0 in
        List.iter (fun i ->
          if px.(i) >= x1 && px.(i) <= x2 && py.(i) >= y1 && py.(i) <= y2 then incr c
        ) nd.ids;
        !c
      end else begin
        let h = nd.s / 2 in
        let rx = [| nd.x; nd.x + h; nd.x; nd.x + h |] in
        let ry = [| nd.y; nd.y; nd.y + h; nd.y + h |] in
        let rs = [| h; nd.s - h; h; nd.s - h |] in
        let tot = ref 0 in
        for k = 0 to 3 do
          let ck = nd.c.(k) in
          if ck <> -1 then begin
            if not (rx.(k) > x2 || rx.(k) + rs.(k) - 1 < x1 || ry.(k) > y2 || ry.(k) + rs.(k) - 1 < y1) then
              tot := !tot + count_pts ck x1 y1 x2 y2
          end
        done;
        !tot
      end
    in

    if !pos < Array.length tokens then begin
      let qq = next_int () in
      for _ = 0 to qq - 1 do
        if !pos < Array.length tokens then begin
          let x1 = next_int () in
          let y1 = next_int () in
          let x2 = next_int () in
          let y2 = next_int () in
          Printf.printf "%d\n" (count_pts 0 x1 y1 x2 y2)
        end
      done
    end
  end
