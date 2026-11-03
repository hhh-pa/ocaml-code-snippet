let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let target = Scanf.scanf " %Ld" (fun x -> x) in
    let v = Array.init n (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
    let poss t =
      if t < 0L then false
      else
        let ti = Int64.to_int t in
        let dp = Array.make (ti + 1) false in
        dp.(0) <- true;
        for i = 0 to n - 1 do
          let x = v.(i) in
          if x >= 0L && x <= t then
            let xi = Int64.to_int x in
            for s = ti downto xi do
              if dp.(s - xi) then dp.(s) <- true
            done
        done;
        dp.(ti)
    in
    let tot = ref 0L in
    for i = 0 to n - 1 do
      tot := Int64.add !tot v.(i)
    done;
    Printf.printf "%s\n" (if poss target then "YES" else "NO");
    let even_tot = Int64.rem !tot 2L = 0L in
    let half_poss = even_tot && poss (Int64.div !tot 2L) in
    Printf.printf "%s\n" (if half_poss then "YES" else "NO")
  with End_of_file -> ()
