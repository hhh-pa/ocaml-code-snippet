let inf = 1000000000

let () =
  try
    let k = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let rq = Array.init m (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
    if k <= 0 then
      Printf.printf "%d\n" m
    else
      let q = Hashtbl.create m in
      for i = 0 to m - 1 do
        let p = rq.(i) in
        let queue =
          match Hashtbl.find_opt q p with
          | Some qu -> qu
          | None ->
            let qu = Queue.create () in
            Hashtbl.add q p qu;
            qu
        in
        Queue.push i queue
      done;
      let cache = Hashtbl.create k in
      let faults = ref 0 in
      for i = 0 to m - 1 do
        let p = rq.(i) in
        let qu = Hashtbl.find q p in
        let _ = Queue.pop qu in
        if not (Hashtbl.mem cache p) then begin
          incr faults;
          if Hashtbl.length cache < k then
            Hashtbl.add cache p ()
          else begin
            let best = ref 0L in
            let bestn = ref (-1) in
            let first = ref true in
            Hashtbl.iter (fun c () ->
              let qu_c = Hashtbl.find q c in
              let nu = if Queue.is_empty qu_c then inf else Queue.peek qu_c in
              if !first || nu > !bestn || (nu = !bestn && c < !best) then begin
                best := c;
                bestn := nu;
                first := false
              end
            ) cache;
            Hashtbl.remove cache !best;
            Hashtbl.add cache p ()
          end
        end
      done;
      Printf.printf "%d\n" !faults
  with End_of_file -> ()
