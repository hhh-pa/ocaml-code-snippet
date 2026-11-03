let inf = 4_000_000_000_000_000_000L

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let s = Scanf.scanf " %d" (fun x -> x) in
    let adj = Array.make n [] in
    for _ = 0 to m - 1 do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      let w = Scanf.scanf " %Ld" (fun x -> x) in
      adj.(u) <- (v, w) :: adj.(u)
    done;
    let dist = Array.make n inf in
    dist.(s) <- 0L;
    let inq = Array.make n false in
    let cnt = Array.make n 0 in
    let par = Array.make n (-1) in
    let q = Queue.create () in
    Queue.add s q;
    inq.(s) <- true;

    let cyc v =
      let x = ref v in
      let found = ref false in
      let steps = ref 0 in
      while !steps <= n && !x <> -1 && not !found do
        x := par.(!x);
        if !x = v then found := true;
        incr steps
      done;
      !found
    in

    let neg_cycle = ref false in
    while not (Queue.is_empty q) && not !neg_cycle do
      let u = Queue.take q in
      inq.(u) <- false;
      let d_u = dist.(u) in
      List.iter (fun (v, w) ->
        if not !neg_cycle then begin
          let cand = Int64.add d_u w in
          if cand < dist.(v) then begin
            dist.(v) <- cand;
            par.(v) <- u;
            if cyc v then
              neg_cycle := true
            else if not inq.(v) then begin
              Queue.add v q;
              inq.(v) <- true;
              cnt.(v) <- cnt.(v) + 1;
              if cnt.(v) >= n then
                neg_cycle := true
            end
          end
        end
      ) adj.(u)
    done;

    if !neg_cycle then
      Printf.printf "NEGATIVE CYCLE\n"
    else begin
      let half_inf = Int64.div inf 2L in
      for i = 0 to n - 1 do
        if i > 0 then Printf.printf " ";
        if dist.(i) >= half_inf then
          Printf.printf "INF"
        else
          Printf.printf "%Ld" dist.(i)
      done;
      Printf.printf "\n"
    end
  with End_of_file -> ()
