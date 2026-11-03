let solve_case () =
  let d = Scanf.scanf " %d" (fun x -> x) in
  let n = 1 lsl d in
  let leaves = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
  let full_cnt = ref 0 in
  let rec mm_full idx depth ismax =
    if depth = 0 then begin
      incr full_cnt;
      leaves.(idx)
    end else begin
      let a = mm_full (idx * 2) (depth - 1) (not ismax) in
      let b = mm_full (idx * 2 + 1) (depth - 1) (not ismax) in
      if ismax then max a b else min a b
    end
  in
  let ab_cnt = ref 0 in
  let rec mm_ab idx depth ismax alpha beta =
    if depth = 0 then begin
      incr ab_cnt;
      leaves.(idx)
    end else if ismax then begin
      let v = mm_ab (idx * 2) (depth - 1) false alpha beta in
      let alpha' = max alpha v in
      if alpha' >= beta then v
      else
        let v2 = mm_ab (idx * 2 + 1) (depth - 1) false alpha' beta in
        max v v2
    end else begin
      let v = mm_ab (idx * 2) (depth - 1) true alpha beta in
      let beta' = min beta v in
      if alpha >= beta' then v
      else
        let v2 = mm_ab (idx * 2 + 1) (depth - 1) true alpha beta' in
        min v v2
    end
  in
  let v1 = mm_full 0 d true in
  let _ = mm_ab 0 d true (-2000000000) 2000000000 in
  Printf.printf "MINIMAX %d FULL %d AB %d\n" v1 !full_cnt !ab_cnt

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
