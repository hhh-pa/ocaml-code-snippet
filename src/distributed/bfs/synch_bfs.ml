let inf = 1000000000

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let adj = Array.make n [] in
    for _ = 0 to m - 1 do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      adj.(u) <- v :: adj.(u);
      adj.(v) <- u :: adj.(v)
    done;
    for i = 0 to n - 1 do
      adj.(i) <- List.rev adj.(i)
    done;
    let root = Scanf.scanf " %d" (fun x -> x) in
    let dist = Array.make n inf in
    let parent = Array.make n (-1) in
    let visited = Array.make n false in
    visited.(root) <- true;
    dist.(root) <- 0;
    let frontier = ref [root] in
    let d = ref 0 in
    while !frontier <> [] do
      let nf = ref [] in
      List.iter (fun u ->
        dist.(u) <- !d;
        List.iter (fun v ->
          if not visited.(v) then begin
            visited.(v) <- true;
            parent.(v) <- u;
            nf := v :: !nf
          end
        ) adj.(u)
      ) !frontier;
      frontier := List.rev !nf;
      incr d
    done;
    for i = 0 to n - 1 do
      let d_val = if dist.(i) = inf then -1 else dist.(i) in
      Printf.printf "%d %d %d\n" i parent.(i) d_val
    done
  with End_of_file -> ()
