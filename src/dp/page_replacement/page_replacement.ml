module Int64Set = Set.Make(Int64)

let () =
  try
    let f = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let rq = Array.init m (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in

    (* FIFO *)
    let ff = ref 0 in
    let fq = Queue.create () in
    let fs = ref Int64Set.empty in
    for i = 0 to m - 1 do
      let p = rq.(i) in
      if f <= 0 then
        incr ff
      else if not (Int64Set.mem p !fs) then begin
        incr ff;
        if Queue.length fq >= f then begin
          let ev = Queue.pop fq in
          fs := Int64Set.remove ev !fs
        end;
        Queue.push p fq;
        fs := Int64Set.add p !fs
      end
    done;

    (* LRU *)
    let lf = ref 0 in
    let lru = ref [] in
    for i = 0 to m - 1 do
      let p = rq.(i) in
      if f <= 0 then
        incr lf
      else if List.mem p !lru then
        lru := (List.filter (fun x -> x <> p) !lru) @ [p]
      else begin
        incr lf;
        let cur =
          if List.length !lru >= f then
            match !lru with
            | [] -> []
            | _ :: rest -> rest
          else
            !lru
        in
        lru := cur @ [p]
      end
    done;

    (* MARKING *)
    let mf = ref 0 in
    let cache = ref Int64Set.empty in
    let marked = ref Int64Set.empty in
    let rng = ref 42L in
    let mod31 = Int64.shift_left 1L 31 in
    let rnd () =
      rng := Int64.rem (Int64.add (Int64.mul 1103515245L !rng) 12345L) mod31;
      !rng
    in
    for i = 0 to m - 1 do
      let p = rq.(i) in
      if f <= 0 then
        incr mf
      else if Int64Set.mem p !cache then
        marked := Int64Set.add p !marked
      else begin
        incr mf;
        if Int64Set.cardinal !cache < f then begin
          cache := Int64Set.add p !cache;
          marked := Int64Set.add p !marked
        end else begin
          if Int64Set.cardinal !marked >= Int64Set.cardinal !cache then
            marked := Int64Set.empty;
          let cands =
            Int64Set.elements !cache
            |> List.filter (fun c -> not (Int64Set.mem c !marked))
            |> Array.of_list
          in
          let r = rnd () in
          let ev_idx = Int64.to_int (Int64.rem r (Int64.of_int (Array.length cands))) in
          let ev = cands.(ev_idx) in
          cache := Int64Set.remove ev !cache;
          marked := Int64Set.remove ev !marked;
          cache := Int64Set.add p !cache;
          marked := Int64Set.add p !marked
        end
      end
    done;

    Printf.printf "%d\n%d\n%d\n" !ff !lf !mf
  with End_of_file -> ()
