type box = { w: int64; d: int64; h: int64 }

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let r = ref [] in
    for _ = 0 to n - 1 do
      let x = Scanf.scanf " %Ld" (fun v -> v) in
      let y = Scanf.scanf " %Ld" (fun v -> v) in
      let z = Scanf.scanf " %Ld" (fun v -> v) in
      let perms = [ (x, y, z); (x, z, y); (y, z, x) ] in
      List.iter (fun (a, b, h) ->
        let w_val, d_val = if a < b then (b, a) else (a, b) in
        r := { w = w_val; d = d_val; h } :: !r
      ) perms
    done;
    let boxes = Array.of_list !r in
    Array.sort (fun a b ->
      let area_a = Int64.mul a.w a.d in
      let area_b = Int64.mul b.w b.d in
      if area_a <> area_b then
        if area_a > area_b then -1 else 1
      else if a.w <> b.w then
        if a.w > b.w then -1 else 1
      else if a.d <> b.d then
        if a.d > b.d then -1 else 1
      else 0
    ) boxes;
    let m = Array.length boxes in
    let dp = Array.init m (fun i -> boxes.(i).h) in
    for i = 0 to m - 1 do
      for j = 0 to i - 1 do
        if boxes.(j).w > boxes.(i).w && boxes.(j).d > boxes.(i).d then
          let cand = Int64.add dp.(j) boxes.(i).h in
          if cand > dp.(i) then dp.(i) <- cand
      done
    done;
    let ans = ref 0L in
    for i = 0 to m - 1 do
      if dp.(i) > !ans then ans := dp.(i)
    done;
    Printf.printf "%Ld\n" !ans
  with End_of_file -> ()
