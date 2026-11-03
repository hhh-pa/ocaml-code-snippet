let inf = 1_000_000_000_000_000_000L

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

    let level = Array.make n (-1) in
    let ptr = Array.make n 0 in

    let bfs () =
      Array.fill level 0 n (-1);
      level.(s) <- 0;
      let q = Queue.create () in
      Queue.add s q;
      while not (Queue.is_empty q) do
        let v = Queue.take q in
        let edges_v = adj.(v) in
        for i = 0 to Array.length edges_v - 1 do
          let e = edges_v.(i) in
          if Int64.sub e.cap e.flow > 0L && level.(e.to_node) = -1 then begin
            level.(e.to_node) <- level.(v) + 1;
            Queue.add e.to_node q
          end
        done
      done;
      level.(t) <> -1
    in

    let rec dfs v pushed =
      if pushed = 0L || v = t then pushed
      else begin
        let edges_v = adj.(v) in
        let len = Array.length edges_v in
        let res = ref 0L in
        while ptr.(v) < len && !res = 0L do
          let cid = ptr.(v) in
          let e = edges_v.(cid) in
          let tr = e.to_node in
          let rem = Int64.sub e.cap e.flow in
          if level.(v) + 1 = level.(tr) && rem > 0L then begin
            let tr_pushed = dfs tr (min pushed rem) in
            if tr_pushed > 0L then begin
              e.flow <- Int64.add e.flow tr_pushed;
              let rev_e = adj.(tr).(e.rev) in
              rev_e.flow <- Int64.sub rev_e.flow tr_pushed;
              res := tr_pushed
            end else
              ptr.(v) <- ptr.(v) + 1
          end else
            ptr.(v) <- ptr.(v) + 1
        done;
        !res
      end
    in

    let flow = ref 0L in
    while bfs () do
      Array.fill ptr 0 n 0;
      let rec push_loop () =
        let pushed = dfs s inf in
        if pushed > 0L then begin
          flow := Int64.add !flow pushed;
          push_loop ()
        end
      in
      push_loop ()
    done;
    Printf.printf "%Ld\n" !flow
  with End_of_file -> ()
