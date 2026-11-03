let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let vals = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    if n = 0 || vals.(0) = -1 then begin
      Printf.printf "0\n";
      exit 0
    end;

    let q = Queue.create () in
    Queue.push (0, 1) q;
    let found = ref false in
    while not (Queue.is_empty q) && not !found do
      let (idx, d) = Queue.pop q in
      if vals.(idx) <> -1 then begin
        let l = 2 * idx + 1 in
        let r = 2 * idx + 2 in
        let left_exists = l < n && vals.(l) <> -1 in
        let right_exists = r < n && vals.(r) <> -1 in
        if not left_exists && not right_exists then begin
          Printf.printf "%d\n" d;
          found := true
        end else begin
          if l < n then Queue.push (l, d + 1) q;
          if r < n then Queue.push (r, d + 1) q
        end
      end
    done;
    if not !found then
      Printf.printf "0\n"
  with End_of_file -> ()
