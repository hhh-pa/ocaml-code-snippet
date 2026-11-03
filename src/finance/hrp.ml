let cvar s items iv =
  let wsum = ref 0.0 in
  List.iter (fun i -> wsum := !wsum +. iv.(i)) items;
  let v = ref 0.0 in
  List.iter (fun i ->
    List.iter (fun j ->
      v := !v +. (iv.(i) /. !wsum) *. (iv.(j) /. !wsum) *. s.(i).(j)
    ) items
  ) items;
  !v

let () =
  let tokens =
    let buf = Buffer.create 4096 in
    (try while true do Buffer.add_channel buf stdin 4096 done with End_of_file -> ());
    let s = Buffer.contents buf in
    let len = String.length s in
    let res = ref [] in
    let i = ref 0 in
    while !i < len do
      while !i < len && (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\r' || s.[!i] = '\n') do incr i done;
      if !i < len then begin
        let st = !i in
        while !i < len && not (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\r' || s.[!i] = '\n') do incr i done;
        res := String.sub s st (!i - st) :: !res
      end
    done;
    Array.of_list (List.rev !res)
  in
  if Array.length tokens >= 1 then begin
    let n = int_of_string tokens.(0) in
    let s = Array.make_matrix n n 0.0 in
    let idx = ref 1 in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        s.(i).(j) <- float_of_string tokens.(!idx);
        incr idx
      done
    done;
    let sd = Array.init n (fun i -> sqrt s.(i).(i)) in
    let d = Array.make_matrix n n 0.0 in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        let rho = s.(i).(j) /. (sd.(i) *. sd.(j)) in
        let rho = max (-1.0) (min 1.0 rho) in
        d.(i).(j) <- sqrt (max 0.0 (0.5 *. (1.0 -. rho)))
      done
    done;
    let total_nodes = 2 * n - 1 in
    let members = Array.make total_nodes [] in
    for i = 0 to n - 1 do members.(i) <- [i] done;
    let alive = Array.make total_nodes false in
    for i = 0 to n - 1 do alive.(i) <- true done;
    let cd = Array.make_matrix total_nodes total_nodes 1e100 in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        cd.(i).(j) <- d.(i).(j)
      done
    done;
    let left_child = Array.make total_nodes (-1) in
    let right_child = Array.make total_nodes (-1) in
    let next_node = ref n in
    for _m = 0 to n - 2 do
      let best = ref 1e100 in
      let bi = ref (-1) in
      let bj = ref (-1) in
      for i = 0 to !next_node - 1 do
        if alive.(i) then
          for j = i + 1 to !next_node - 1 do
            if alive.(j) && cd.(i).(j) < !best then begin
              best := cd.(i).(j);
              bi := i;
              bj := j
            end
          done
      done;
      alive.(!bi) <- false;
      alive.(!bj) <- false;
      alive.(!next_node) <- true;
      left_child.(!next_node) <- !bi;
      right_child.(!next_node) <- !bj;
      members.(!next_node) <- members.(!bi) @ members.(!bj);
      for i = 0 to !next_node - 1 do
        if alive.(i) then begin
          let dd = min cd.(!bi).(i) cd.(!bj).(i) in
          cd.(!next_node).(i) <- dd;
          cd.(i).(!next_node) <- dd
        end
      done;
      incr next_node
    done;
    let order = ref [] in
    let tmp = ref [2 * n - 2] in
    while !tmp <> [] do
      let u = List.hd !tmp in
      tmp := List.tl !tmp;
      if u < n then order := u :: !order
      else tmp := left_child.(u) :: right_child.(u) :: !tmp
    done;
    let order = List.rev !order in
    let iv = Array.init n (fun i -> 1.0 /. s.(i).(i)) in
    let w = Array.make n 1.0 in
    let work = ref [order] in
    while !work <> [] do
      let cur = List.hd !work in
      work := List.tl !work;
      let len = List.length cur in
      if len >= 2 then begin
        let mid = len / 2 in
        let cur_arr = Array.of_list cur in
        let lc = Array.to_list (Array.sub cur_arr 0 mid) in
        let rc = Array.to_list (Array.sub cur_arr mid (len - mid)) in
        let vl = cvar s lc iv in
        let vr = cvar s rc iv in
        let a = 1.0 -. vl /. (vl +. vr) in
        List.iter (fun i -> w.(i) <- w.(i) *. a) lc;
        List.iter (fun i -> w.(i) <- w.(i) *. (1.0 -. a)) rc;
        work := rc :: lc :: !work
      end
    done;
    let ws = Array.fold_left (+.) 0.0 w in
    for i = 0 to n - 1 do w.(i) <- w.(i) /. ws done;
    Printf.printf "ORDER:";
    List.iter (fun x -> Printf.printf " %d" x) order;
    Printf.printf "\nWEIGHTS:";
    Array.iter (fun x -> Printf.printf " %.6f" (x +. 0.0)) w;
    Printf.printf "\n"
  end
