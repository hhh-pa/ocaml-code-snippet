type pt = { x : int64; y : int64 }

type pst_node = {
  p : pt;
  xmin : int64;
  xmax : int64;
  left : int;
  right : int;
}

let cmp_pt a b =
  let c = Int64.compare a.x b.x in
  if c <> 0 then c else Int64.compare a.y b.y

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
    let pts = Array.init n (fun _ ->
      let x = next_int64 () in
      let y = next_int64 () in
      { x; y }
    ) in
    let xs = Array.init n (fun i -> i) in
    Array.sort (fun a b -> cmp_pt pts.(a) pts.(b)) xs;

    let tree = ref [] in

    let rec build ids =
      match ids with
      | [] -> -1
      | _ ->
        let ids_arr = Array.of_list ids in
        let best = ref 0 in
        for i = 1 to Array.length ids_arr - 1 do
          if pts.(ids_arr.(i)).y > pts.(ids_arr.(!best)).y then best := i
        done;
        let pid = ids_arr.(!best) in
        let left_ids = ref [] in
        let right_ids = ref [] in
        for i = 0 to Array.length ids_arr - 1 do
          if i < !best then left_ids := ids_arr.(i) :: !left_ids
          else if i > !best then right_ids := ids_arr.(i) :: !right_ids
        done;
        let l = build (List.rev !left_ids) in
        let r = build (List.rev !right_ids) in
        let xmin = ref pts.(pid).x in
        let xmax = ref pts.(pid).x in
        let tree_arr = Array.of_list (List.rev !tree) in
        if l >= 0 then begin
          let c = tree_arr.(l) in
          if c.xmin < !xmin then xmin := c.xmin;
          if c.xmax > !xmax then xmax := c.xmax
        end;
        if r >= 0 then begin
          let c = tree_arr.(r) in
          if c.xmin < !xmin then xmin := c.xmin;
          if c.xmax > !xmax then xmax := c.xmax
        end;
        let node_id = List.length !tree in
        let nd = { p = pts.(pid); xmin = !xmin; xmax = !xmax; left = l; right = r } in
        tree := nd :: !tree;
        node_id
    in

    let root = if n > 0 then build (Array.to_list xs) else -1 in
    let tree_arr = Array.of_list (List.rev !tree) in

    let rec query v x1 x2 y0 out_list =
      if v >= 0 then begin
        let nd = tree_arr.(v) in
        if nd.p.y >= y0 && x1 <= nd.p.x && nd.p.x <= x2 then
          out_list := nd.p :: !out_list;
        if nd.left >= 0 then begin
          let c = tree_arr.(nd.left) in
          if c.xmax >= x1 && c.xmin <= x2 && c.p.y >= y0 then
            query nd.left x1 x2 y0 out_list
        end;
        if nd.right >= 0 then begin
          let c = tree_arr.(nd.right) in
          if c.xmax >= x1 && c.xmin <= x2 && c.p.y >= y0 then
            query nd.right x1 x2 y0 out_list
        end
      end
    in

    if !pos < Array.length tokens then begin
      let q = next_int () in
      for _ = 0 to q - 1 do
        if !pos < Array.length tokens then begin
          let x1 = next_int64 () in
          let x2 = next_int64 () in
          let y0 = next_int64 () in
          let found = ref [] in
          query root x1 x2 y0 found;
          let found_arr = Array.of_list !found in
          Array.sort cmp_pt found_arr;
          Printf.printf "%d\n" (Array.length found_arr);
          if Array.length found_arr > 0 then begin
            for k = 0 to Array.length found_arr - 1 do
              if k > 0 then print_string " ";
              Printf.printf "%Ld,%Ld" found_arr.(k).x found_arr.(k).y
            done;
            print_newline ()
          end
        end
      done
    end
  end
