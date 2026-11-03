let solve_case () =
  let n = Scanf.scanf " %d" (fun x -> x) in
  let m = Scanf.scanf " %d" (fun x -> x) in
  let adj = Array.make n [] in
  for _ = 0 to m - 1 do
    let u = Scanf.scanf " %d" (fun x -> x) - 1 in
    let v = Scanf.scanf " %d" (fun x -> x) - 1 in
    adj.(u) <- v :: adj.(u);
    adj.(v) <- u :: adj.(v)
  done;
  let start = Scanf.scanf " %d" (fun x -> x) - 1 in
  let dist = Array.make n (-1) in
  dist.(start) <- 0;
  let q = Queue.create () in
  Queue.push start q;
  while not (Queue.is_empty q) do
    let cur = Queue.pop q in
    let nd = dist.(cur) + 6 in
    List.iter (fun nxt ->
      if dist.(nxt) = -1 || nd < dist.(nxt) then begin
        dist.(nxt) <- nd;
        Queue.push nxt q
      end
    ) adj.(cur)
  done;
  for i = 0 to n - 1 do
    if i <> start then begin
      Printf.printf "%d" dist.(i);
      if i <> n - 1 then Printf.printf " "
    end
  done;
  Printf.printf "\n"

let () =
  try
    let queries = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to queries - 1 do
      solve_case ()
    done
  with End_of_file -> ()
