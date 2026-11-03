let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let adj = Array.make n [] in
    for _ = 0 to m - 1 do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      if u <> v then begin
        adj.(u) <- v :: adj.(u);
        adj.(v) <- u :: adj.(v)
      end
    done;

    for u = 0 to n - 1 do
      let s = List.sort_uniq compare adj.(u) in
      adj.(u) <- s
    done;

    let match_ = Array.make n (-1) in
    let p = Array.make n (-1) in
    let base = Array.init n (fun i -> i) in
    let used = Array.make n 0 in
    let bloss = Array.make n 0 in
    let used_lca = Array.make n 0 in

    let lca a b =
      Array.fill used_lca 0 n 0;
      let curr_a = ref a in
      let stop_a = ref false in
      while not !stop_a do
        curr_a := base.(!curr_a);
        used_lca.(!curr_a) <- 1;
        if match_.(!curr_a) = -1 then
          stop_a := true
        else
          curr_a := p.(match_.(!curr_a))
      done;
      let curr_b = ref b in
      let res = ref (-1) in
      while !res = -1 do
        curr_b := base.(!curr_b);
        if used_lca.(!curr_b) = 1 then
          res := !curr_b
        else if match_.(!curr_b) = -1 then
          res := -1
        else
          curr_b := p.(match_.(!curr_b))
      done;
      !res
    in

    let mark_path v b child =
      let curr_v = ref v in
      let curr_child = ref child in
      while base.(!curr_v) <> b do
        bloss.(base.(!curr_v)) <- 1;
        bloss.(base.(match_.(!curr_v))) <- 1;
        p.(!curr_v) <- !curr_child;
        curr_child := match_.(!curr_v);
        curr_v := p.(match_.(!curr_v))
      done
    in

    let find_path root =
      Array.fill p 0 n (-1);
      for i = 0 to n - 1 do base.(i) <- i done;
      Array.fill used 0 n 0;
      used.(root) <- 1;
      let q = Queue.create () in
      Queue.add root q;
      let found = ref false in

      while not (Queue.is_empty q) && not !found do
        let v = Queue.take q in
        List.iter (fun to_node ->
          if not !found && base.(v) <> base.(to_node) && match_.(v) <> to_node then begin
            if to_node = root || (match_.(to_node) <> -1 && p.(match_.(to_node)) <> -1) then begin
              let cur = lca v to_node in
              Array.fill bloss 0 n 0;
              mark_path v cur to_node;
              mark_path to_node cur v;
              for i = 0 to n - 1 do
                if bloss.(base.(i)) = 1 then begin
                  base.(i) <- cur;
                  if used.(i) = 0 then begin
                    used.(i) <- 1;
                    Queue.add i q
                  end
                end
              done
            end else if p.(to_node) = -1 then begin
              p.(to_node) <- v;
              if match_.(to_node) = -1 then begin
                let cur = ref to_node in
                while !cur <> -1 do
                  let prev = p.(!cur) in
                  let nxt = if prev <> -1 then match_.(prev) else -1 in
                  match_.(!cur) <- prev;
                  if prev <> -1 then match_.(prev) <- !cur;
                  cur := nxt
                done;
                found := true
              end else begin
                let nxt = match_.(to_node) in
                used.(nxt) <- 1;
                Queue.add nxt q
              end
            end
          end
        ) adj.(v)
      done;
      !found
    in

    for v = 0 to n - 1 do
      if match_.(v) = -1 then
        ignore (find_path v)
    done;

    let prs = ref [] in
    for i = 0 to n - 1 do
      if match_.(i) <> -1 && i < match_.(i) then
        prs := (i, match_.(i)) :: !prs
    done;
    let sorted_prs = List.sort compare !prs in
    Printf.printf "%d\n" (List.length sorted_prs);
    List.iter (fun (u, v) -> Printf.printf "%d %d\n" u v) sorted_prs
  with End_of_file -> ()
