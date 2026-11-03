let inf = 1000000000

let () =
  try
    let s1 = Scanf.scanf " %s" (fun x -> x) in
    let s2 = Scanf.scanf " %s" (fun x -> x) in
    let k = Scanf.scanf " %d" (fun x -> x) in
    let n = String.length s1 in
    let m = String.length s2 in
    let dp = Array.make_matrix (n + 1) (m + 1) inf in
    dp.(0).(0) <- 0;
    for i = 0 to n do
      let low_j = max 0 (i - k) in
      let high_j = min m (i + k) in
      for j = low_j to high_j do
        if not (i = 0 && j = 0) then begin
          let best = ref inf in
          if i > 0 && abs ((i - 1) - j) <= k then
            best := min !best (dp.(i - 1).(j) + 1);
          if j > 0 && abs (i - (j - 1)) <= k then
            best := min !best (dp.(i).(j - 1) + 1);
          if i > 0 && j > 0 && abs ((i - 1) - (j - 1)) <= k then begin
            let cost = if s1.[i - 1] = s2.[j - 1] then 0 else 1 in
            best := min !best (dp.(i - 1).(j - 1) + cost)
          end;
          dp.(i).(j) <- !best
        end
      done
    done;
    let ans = dp.(n).(m) in
    Printf.printf "%d\n" (if ans >= inf then -1 else ans)
  with End_of_file -> ()
