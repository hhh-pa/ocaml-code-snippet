let inf = 1_000_000_000

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let e = Scanf.scanf " %d" (fun x -> x) in
    let adj = Array.make (n + 1) [] in
    for _ = 1 to e do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      adj.(u) <- v :: adj.(u)
    done;
    let pair_u = Array.make (n + 1) 0 in
    let pair_v = Array.make (m + 1) 0 in
    let dist = Array.make (n + 1) 0 in
    let q = Queue.create () in

    let bfs () =
      Queue.clear q;
      for u = 1 to n do
        if pair_u.(u) = 0 then begin
          dist.(u) <- 0;
          Queue.push u q
        end else
          dist.(u) <- inf
      done;
      dist.(0) <- inf;
      while not (Queue.is_empty q) do
        let u = Queue.pop q in
        if dist.(u) < dist.(0) then begin
          List.iter (fun v ->
            let next_u = pair_v.(v) in
            if dist.(next_u) = inf then begin
              dist.(next_u) <- dist.(u) + 1;
              Queue.push next_u q
            end
          ) adj.(u)
        end
      done;
      dist.(0) <> inf
    in

    let rec dfs u =
      if u <> 0 then begin
        let found = ref false in
        let rec try_edges = function
          | [] -> false
          | v :: rest ->
            let next_u = pair_v.(v) in
            if dist.(next_u) = dist.(u) + 1 && dfs next_u then begin
              pair_v.(v) <- u;
              pair_u.(u) <- v;
              true
            end else
              try_edges rest
        in
        if try_edges adj.(u) then true
        else begin
          dist.(u) <- inf;
          false
        end
      end else
        true
    in

    let matching = ref 0 in
    while bfs () do
      for u = 1 to n do
        if pair_u.(u) = 0 && dfs u then
          incr matching
      done
    done;

    Printf.printf "%d\n" !matching;
    for u = 1 to n do
      if pair_u.(u) <> 0 then
        Printf.printf "%d %d\n" u pair_u.(u)
    done
  with End_of_file -> ()
