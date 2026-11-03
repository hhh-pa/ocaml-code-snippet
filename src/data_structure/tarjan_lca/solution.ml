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
    let n = next_int () in
    let adj = Array.init (n + 1) (fun _ -> ref []) in
    for _ = 0 to n - 2 do
      let u = next_int () in
      let v = next_int () in
      adj.(u) := v :: !(adj.(u));
      adj.(v) := u :: !(adj.(v))
    done;
    let q = next_int () in
    let queries = Array.init (n + 1) (fun _ -> ref []) in
    let ans = Array.make q 0 in
    for i = 0 to q - 1 do
      let a = next_int () in
      let b = next_int () in
      queries.(a) := (i, b) :: !(queries.(a));
      queries.(b) := (i, a) :: !(queries.(b))
    done;
    let parent = Array.init (n + 1) (fun i -> i) in
    let ancestor = Array.init (n + 1) (fun i -> i) in
    let visited = Array.make (n + 1) false in
    let rec find x =
      if parent.(x) = x then x
      else begin
        parent.(x) <- find parent.(x);
        parent.(x)
      end
    in
    let rec dfs u p =
      parent.(u) <- u;
      ancestor.(u) <- u;
      List.iter (fun v ->
        if v <> p then begin
          dfs v u;
          parent.(find v) <- find u;
          ancestor.(find u) <- u
        end
      ) !(adj.(u));
      visited.(u) <- true;
      List.iter (fun (q_idx, v) ->
        if visited.(v) then ans.(q_idx) <- ancestor.(find v)
      ) !(queries.(u))
    in
    if n > 0 then dfs 1 0;
    for i = 0 to q - 1 do
      Printf.printf "%d\n" ans.(i)
    done
  end
