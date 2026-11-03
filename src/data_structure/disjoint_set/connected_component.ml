let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let parent = Array.init n (fun i -> i) in
    let rec find i =
      if parent.(i) = i then i
      else begin
        let root = find parent.(i) in
        parent.(i) <- root;
        root
      end
    in
    let root_number = ref n in
    let rec loop () =
      let u = Scanf.scanf " %d" (fun x -> x) - 1 in
      let v = Scanf.scanf " %d" (fun x -> x) - 1 in
      let u_root = find u in
      let v_root = find v in
      if u_root <> v_root then begin
        parent.(v_root) <- u_root;
        decr root_number;
        Printf.printf "%d\n" !root_number
      end else begin
        Printf.printf "%d CYCLE FORMED!\n" !root_number
      end;
      loop ()
    in
    loop ()
  with End_of_file -> ()
