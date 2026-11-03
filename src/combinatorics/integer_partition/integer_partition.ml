let rec gen rem maxv pre =
  if rem = 0 then begin
    let l = List.rev pre in
    List.iteri (fun i v ->
      if i > 0 then print_char ' ';
      print_int v
    ) l;
    print_newline ()
  end else begin
    let top = min maxv rem in
    for v = top downto 1 do
      gen (rem - v) v (v :: pre)
    done
  end

let () =
  try
    let mode = Scanf.scanf " %s" (fun s -> s) in
    if mode = "G" then begin
      let n = Scanf.scanf " %d" (fun x -> x) in
      gen n n []
    end else begin
      let k = Scanf.scanf " %d" (fun x -> x) in
      let p = Array.init k (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
      let c = ref [] in
      for j = 0 to p.(0) - 1 do
        let cnt = ref 0 in
        for i = 0 to k - 1 do
          if p.(i) > j then incr cnt
        done;
        c := !cnt :: !c
      done;
      let c = List.rev !c in
      List.iteri (fun i v ->
        if i > 0 then print_char ' ';
        print_int v
      ) c;
      print_newline ()
    end
  with End_of_file -> ()
