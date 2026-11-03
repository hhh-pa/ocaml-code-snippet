let inf = 1000000000

let solve_case () =
  let n = Scanf.scanf " %d" (fun x -> x) in
  let m = Scanf.scanf " %d" (fun x -> x) in
  let s = Scanf.scanf " %d" (fun x -> x) in
  let goal = Scanf.scanf " %d" (fun x -> x) in

  let adj = Array.make n [] in
  for _ = 0 to m - 1 do
    let u = Scanf.scanf " %d" (fun x -> x) in
    let v = Scanf.scanf " %d" (fun x -> x) in
    let w = Scanf.scanf " %d" (fun x -> x) in
    adj.(u) <- (v, w) :: adj.(u);
    adj.(v) <- (u, w) :: adj.(v)
  done;
  for i = 0 to n - 1 do
    adj.(i) <- List.sort compare adj.(i)
  done;

  let hval = Array.make n 0 in
  for i = 0 to n - 1 do
    hval.(i) <- Scanf.scanf " %d" (fun x -> x)
  done;

  let path = ref [s] in
  let onpath = Array.make n false in
  onpath.(s) <- true;

  let rec dfs v g bound =
    let f = g + hval.(v) in
    if f > bound then f
    else if v = goal then -1
    else begin
      let best = ref inf in
      let found = ref false in
      let rec loop = function
        | [] -> ()
        | (to_node, w) :: rest ->
            if onpath.(to_node) then loop rest
            else begin
              onpath.(to_node) <- true;
              path := to_node :: !path;
              let r = dfs to_node (g + w) bound in
              if r < 0 then found := true
              else begin
                path := List.tl !path;
                onpath.(to_node) <- false;
                if r < !best then best := r;
                loop rest
              end
            end
      in
      loop adj.(v);
      if !found then -1
      else !best
    end
  in

  let dist = ref (-1) in
  let final_path = ref [] in

  if s = goal then begin
    dist := 0;
    final_path := [s]
  end else begin
    let bound = ref hval.(s) in
    let stop = ref false in
    let iter = ref 0 in
    while !iter < 100000 && not !stop do
      let r = dfs s 0 !bound in
      if r < 0 then begin
        let p = List.rev !path in
        let g2 = ref 0 in
        let p_arr = Array.of_list p in
        for i = 1 to Array.length p_arr - 1 do
          let u = p_arr.(i - 1) in
          let v = p_arr.(i) in
          let rec find_w = function
            | [] -> ()
            | (nxt, w) :: rest ->
                if nxt = v then g2 := !g2 + w
                else find_w rest
          in
          find_w adj.(u)
        done;
        dist := !g2;
        final_path := p;
        stop := true
      end else if r >= inf then
        stop := true
      else
        bound := r;
      incr iter
    done
  end;

  if !dist < 0 then
    Printf.printf "IDA -1\n"
  else begin
    Printf.printf "IDA %d" !dist;
    List.iter (fun v -> Printf.printf " %d" v) !final_path;
    Printf.printf "\n"
  end

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
