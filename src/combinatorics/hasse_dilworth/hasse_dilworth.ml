let rec bpm u n reach match_r seen =
  let rec loop v =
    if v = n then false
    else if u <> v && reach.(u).(v) && not seen.(v) then begin
      seen.(v) <- true;
      if match_r.(v) < 0 || bpm match_r.(v) n reach match_r seen then begin
        match_r.(v) <- u;
        true
      end else
        loop (v + 1)
    end else
      loop (v + 1)
  in
  loop 0

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let reach = Array.make_matrix n n false in
    for i = 0 to n - 1 do
      reach.(i).(i) <- true
    done;
    for _ = 1 to m do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      reach.(u).(v) <- true
    done;
    for k = 0 to n - 1 do
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          if reach.(i).(k) && reach.(k).(j) then reach.(i).(j) <- true
        done
      done
    done;
    let hasse = ref [] in
    for u = 0 to n - 1 do
      for v = 0 to n - 1 do
        if u <> v && reach.(u).(v) then begin
          let cover = ref true in
          for w = 0 to n - 1 do
            if w <> u && w <> v && reach.(u).(w) && reach.(w).(v) then
              cover := false
          done;
          if !cover then hasse := (u, v) :: !hasse
        end
      done
    done;
    let hasse = List.sort compare !hasse in
    let adj = Array.make n [] in
    let indeg = Array.make n 0 in
    List.iter (fun (u, v) ->
      adj.(u) <- v :: adj.(u);
      indeg.(v) <- indeg.(v) + 1
    ) hasse;
    for u = 0 to n - 1 do
      adj.(u) <- List.sort compare adj.(u)
    done;
    let avail = ref [] in
    for i = 0 to n - 1 do
      if indeg.(i) = 0 then avail := i :: !avail
    done;
    avail := List.sort compare !avail;
    let topo = ref [] in
    while !avail <> [] do
      let u = List.hd !avail in
      avail := List.tl !avail;
      topo := u :: !topo;
      List.iter (fun v ->
        indeg.(v) <- indeg.(v) - 1;
        if indeg.(v) = 0 then begin
          avail := List.sort compare (v :: !avail)
        end
      ) adj.(u)
    done;
    let topo = List.rev !topo in
    let h = Array.make n 1 in
    List.iter (fun v ->
      for u = 0 to n - 1 do
        if u <> v && reach.(u).(v) then
          h.(v) <- max h.(v) (h.(u) + 1)
      done
    ) topo;
    let height = ref 0 in
    for i = 0 to n - 1 do
      if h.(i) > !height then height := h.(i)
    done;
    let match_r = Array.make n (-1) in
    let matching = ref 0 in
    for u = 0 to n - 1 do
      let seen = Array.make n false in
      if bpm u n reach match_r seen then incr matching
    done;
    Printf.printf "%d\n" (List.length hasse);
    List.iter (fun (u, v) ->
      Printf.printf "%d %d\n" u v
    ) hasse;
    Printf.printf "%d\n" !height;
    Printf.printf "%d\n" (n - !matching)
  with End_of_file -> ()
