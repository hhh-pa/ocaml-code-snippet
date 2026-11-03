let inf = 1000000000

type succ = { mutable f : int; to_node : int; w : int }

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
  let hval = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
  let cur_path = ref [s] in
  let onpath = Array.make n false in
  onpath.(s) <- true;

  let rec rbfs v g f_limit =
    if v = goal then -1
    else begin
      let succ_list = ref [] in
      List.iter (fun (to_n, w) ->
        if not onpath.(to_n) then
          succ_list := { f = w + g + hval.(to_n); to_node = to_n; w } :: !succ_list
      ) adj.(v);
      if !succ_list = [] then inf
      else begin
        let succs = Array.of_list !succ_list in
        let fv = g + hval.(v) in
        for i = 0 to Array.length succs - 1 do
          succs.(i).f <- max succs.(i).f fv
        done;
        let cmp_succ a b =
          if a.f <> b.f then compare a.f b.f
          else compare a.to_node b.to_node
        in
        let rec loop () =
          Array.sort cmp_succ succs;
          let best = succs.(0).f in
          if best >= inf || best > f_limit then best
          else begin
            let alt = if Array.length succs > 1 then succs.(1).f else inf in
            let to_n = succs.(0).to_node in
            let w = succs.(0).w in
            onpath.(to_n) <- true;
            cur_path := to_n :: !cur_path;
            let r = rbfs to_n (g + w) (min f_limit alt) in
            if r < 0 then -1
            else begin
              cur_path := List.tl !cur_path;
              onpath.(to_n) <- false;
              succs.(0).f <- r;
              loop ()
            end
          end
        in
        loop ()
      end
    end
  in

  if s = goal then begin
    Printf.printf "RBFS 0 %d\n" s
  end else begin
    let r = rbfs s 0 inf in
    if r < 0 then begin
      let path = List.rev !cur_path in
      let path_arr = Array.of_list path in
      let g2 = ref 0 in
      for i = 1 to Array.length path_arr - 1 do
        let a = path_arr.(i - 1) in
        let b = path_arr.(i) in
        let found = ref false in
        List.iter (fun (to_n, w) ->
          if not !found && to_n = b then begin
            g2 := !g2 + w;
            found := true
          end
        ) adj.(a)
      done;
      Printf.printf "RBFS %d" !g2;
      for i = 0 to Array.length path_arr - 1 do
        Printf.printf " %d" path_arr.(i)
      done;
      print_newline ()
    end else
      Printf.printf "RBFS -1\n"
  end

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
