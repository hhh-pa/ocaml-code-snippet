let inf = 4000000000000000000L

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let d = Array.make_matrix n n 0L in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        d.(i).(j) <- Scanf.scanf " %Ld" (fun x -> x)
      done
    done;
    if n = 1 then
      Printf.printf "0\n"
    else
      let big_n = 1 lsl n in
      let dp = Array.make_matrix big_n n inf in
      dp.(1).(0) <- 0L;
      for mask = 0 to big_n - 1 do
        if mask land 1 <> 0 then begin
          for u = 0 to n - 1 do
            let cur = dp.(mask).(u) in
            if (mask land (1 lsl u)) <> 0 && cur <> inf then begin
              for v = 1 to n - 1 do
                if (mask land (1 lsl v)) = 0 then begin
                  let nm = mask lor (1 lsl v) in
                  let cand = Int64.add cur d.(u).(v) in
                  if cand < dp.(nm).(v) then
                    dp.(nm).(v) <- cand
                end
              done
            end
          done
        end
      done;
      let full = big_n - 1 in
      let ans = ref inf in
      for u = 0 to n - 1 do
        if dp.(full).(u) <> inf then
          let cand = Int64.add dp.(full).(u) d.(u).(0) in
          if cand < !ans then ans := cand
      done;
      Printf.printf "%Ld\n" !ans
  with End_of_file -> ()
