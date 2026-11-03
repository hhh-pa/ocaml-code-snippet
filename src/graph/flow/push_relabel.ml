type edge = {
  to_node : int;
  rev : int;
  mutable cap : int64;
  mutable flow : int64;
}

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let s = Scanf.scanf " %d" (fun x -> x) in
    let t = Scanf.scanf " %d" (fun x -> x) in

    let deg = Array.make n 0 in
    let input_edges = Array.init m (fun _ ->
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      let cap = Scanf.scanf " %Ld" (fun x -> x) in
      deg.(u) <- deg.(u) + 1;
      deg.(v) <- deg.(v) + 1;
      (u, v, cap)
    ) in

    let dummy_edge = { to_node = 0; rev = 0; cap = 0L; flow = 0L } in
    let adj = Array.init n (fun i -> Array.make deg.(i) dummy_edge) in
    let head = Array.make n 0 in
    Array.iter (fun (u, v, cap) ->
      let pos_u = head.(u) in
      let pos_v = head.(v) in
      adj.(u).(pos_u) <- { to_node = v; rev = pos_v; cap; flow = 0L };
      adj.(v).(pos_v) <- { to_node = u; rev = pos_u; cap = 0L; flow = 0L };
      head.(u) <- pos_u + 1;
      head.(v) <- pos_v + 1
    ) input_edges;

    let excess = Array.make n 0L in
    let height = Array.make n 0 in
    let count = Array.make (2 * n) 0 in
    let in_queue = Array.make n false in
    let active_q = Queue.create () in

    let enqueue u =
      if not in_queue.(u) && u <> s && u <> t && excess.(u) > 0L then begin
        in_queue.(u) <- true;
        Queue.add u active_q
      end
    in

    let push u edge =
      let rem = Int64.sub edge.cap edge.flow in
      let amt = min excess.(u) rem in
      if amt > 0L && height.(u) = height.(edge.to_node) + 1 then begin
        edge.flow <- Int64.add edge.flow amt;
        let rev_edge = adj.(edge.to_node).(edge.rev) in
        rev_edge.flow <- Int64.sub rev_edge.flow amt;
        excess.(u) <- Int64.sub excess.(u) amt;
        excess.(edge.to_node) <- Int64.add excess.(edge.to_node) amt;
        enqueue edge.to_node
      end
    in

    let relabel u =
      let min_h = ref (2 * n) in
      let edges_u = adj.(u) in
      for i = 0 to Array.length edges_u - 1 do
        let edge = edges_u.(i) in
        if Int64.sub edge.cap edge.flow > 0L then begin
          let h_to = height.(edge.to_node) in
          if h_to < !min_h then min_h := h_to
        end
      done;
      if !min_h < 2 * n then begin
        let old_h = height.(u) in
        count.(old_h) <- count.(old_h) - 1;
        height.(u) <- !min_h + 1;
        count.(height.(u)) <- count.(height.(u)) + 1
      end
    in

    let discharge u =
      let idx = ref 0 in
      let edges_u = adj.(u) in
      let len = Array.length edges_u in
      while excess.(u) > 0L do
        if !idx < len then begin
          let edge = edges_u.(!idx) in
          let rem = Int64.sub edge.cap edge.flow in
          if rem > 0L && height.(u) = height.(edge.to_node) + 1 then
            push u edge
          else
            incr idx
        end else begin
          relabel u;
          idx := 0
        end
      done
    in

    height.(s) <- n;
    count.(n) <- 1;
    count.(0) <- n - 1;

    let edges_s = adj.(s) in
    for i = 0 to Array.length edges_s - 1 do
      let edge = edges_s.(i) in
      let amt = edge.cap in
      if amt > 0L then begin
        edge.flow <- Int64.add edge.flow amt;
        let rev_edge = adj.(edge.to_node).(edge.rev) in
        rev_edge.flow <- Int64.sub rev_edge.flow amt;
        excess.(edge.to_node) <- Int64.add excess.(edge.to_node) amt;
        excess.(s) <- Int64.sub excess.(s) amt;
        enqueue edge.to_node
      end
    done;

    while not (Queue.is_empty active_q) do
      let u = Queue.take active_q in
      in_queue.(u) <- false;
      discharge u
    done;

    Printf.printf "%Ld\n" excess.(t)
  with End_of_file -> ()
