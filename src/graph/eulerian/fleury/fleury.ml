let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let adj = Array.make n [] in
    for i = 0 to m - 1 do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      adj.(u) <- (v, i) :: adj.(u);
      adj.(v) <- (u, i) :: adj.(v)
    done;
    let odd = ref [] in
    for i = n - 1 downto 0 do
      if List.length adj.(i) mod 2 <> 0 then
        odd := i :: !odd
    done;
    if List.length !odd <> 0 && List.length !odd <> 2 then begin
      Printf.printf "NO EULERIAN TRAIL\n";
      exit 0
    end;
    let src = ref (-1) in
    let i = ref 0 in
    while !src = -1 && !i < n do
      if adj.(!i) <> [] then src := !i;
      incr i
    done;
    if !src = -1 then begin
      Printf.printf "0\n";
      exit 0
    end;
    let alive = Array.make m true in
    let sn = Array.make n false in
    sn.(!src) <- true;
    let st = ref [!src] in
    while !st <> [] do
      let u = List.hd !st in
      st := List.tl !st;
      List.iter (fun (w, _) ->
        if not sn.(w) then begin
          sn.(w) <- true;
          st := w :: !st
        end
      ) adj.(u)
    done;
    let connected = ref true in
    for j = 0 to n - 1 do
      if adj.(j) <> [] && not sn.(j) then connected := false
    done;
    if not !connected then begin
      Printf.printf "NO EULERIAN TRAIL\n";
      exit 0
    end;

    let reach s =
      let visited = Array.make n false in
      visited.(s) <- true;
      let stack = ref [s] in
      let count = ref 0 in
      while !stack <> [] do
        let u = List.hd !stack in
        stack := List.tl !stack;
        incr count;
        List.iter (fun (w, e) ->
          if alive.(e) && not visited.(w) then begin
            visited.(w) <- true;
            stack := w :: !stack
          end
        ) adj.(u)
      done;
      !count
    in

    let start = match !odd with [] -> !src | h :: _ -> h in
    let trail = ref [start] in
    let cur_u = ref start in
    let keep_going = ref true in
    while !keep_going do
      let cands = List.filter (fun (_, e) -> alive.(e)) adj.(!cur_u) in
      if cands = [] then
        keep_going := false
      else begin
        let sorted = List.sort (fun (w1, e1) (w2, e2) ->
          let c = compare w1 w2 in
          if c <> 0 then c else compare e1 e2
        ) cands in
        let chosen_w = ref (fst (List.hd sorted)) in
        let chosen_e = ref (snd (List.hd sorted)) in
        if List.length sorted > 1 then begin
          let base = reach !cur_u in
          let found = ref false in
          List.iter (fun (nw, ne) ->
            if not !found then begin
              alive.(ne) <- false;
              if reach !cur_u >= base then begin
                alive.(ne) <- true;
                chosen_w := nw;
                chosen_e := ne;
                found := true
              end else
                alive.(ne) <- true
            end
          ) sorted
        end;
        alive.(!chosen_e) <- false;
        trail := !chosen_w :: !trail;
        cur_u := !chosen_w
      end
    done;
    let any_alive = ref false in
    for k = 0 to m - 1 do
      if alive.(k) then any_alive := true
    done;
    if !any_alive then
      Printf.printf "NO EULERIAN TRAIL\n"
    else begin
      let rev_trail = List.rev !trail in
      let first = ref true in
      List.iter (fun x ->
        if not !first then print_string " ";
        print_int x;
        first := false
      ) rev_trail;
      print_newline ()
    end
  with End_of_file -> ()
