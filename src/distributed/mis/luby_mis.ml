let pri u =
  let u_ll = Int64.of_int (u + 1) in
  let prod = Int64.mul u_ll 2654435761L in
  let shifted = Int64.shift_right_logical prod 16 in
  Int64.logand shifted 0xFFFFFFFFL

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let adj = Array.make n [] in
    for _ = 0 to m - 1 do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      adj.(u) <- v :: adj.(u);
      adj.(v) <- u :: adj.(v)
    done;
    let removed = Array.make n false in
    let in_mis = Array.make n false in
    let continue_loop = ref true in
    while !continue_loop do
      let cands = ref [] in
      for u = 0 to n - 1 do
        if not removed.(u) then begin
          let mx = ref true in
          let pri_u = pri u in
          List.iter (fun v ->
            if not removed.(v) && pri v > pri_u then
              mx := false
          ) adj.(u);
          if !mx then cands := u :: !cands
        end
      done;
      if !cands = [] then
        continue_loop := false
      else begin
        List.iter (fun u ->
          in_mis.(u) <- true;
          removed.(u) <- true;
          List.iter (fun v -> removed.(v) <- true) adj.(u)
        ) !cands
      end
    done;
    let res = ref [] in
    for i = 0 to n - 1 do
      if in_mis.(i) then res := i :: !res
    done;
    let res_arr = Array.of_list (List.rev !res) in
    Array.sort compare res_arr;
    let len = Array.length res_arr in
    Printf.printf "%d\n" len;
    for i = 0 to len - 1 do
      if i > 0 then Printf.printf " ";
      Printf.printf "%d" res_arr.(i)
    done;
    if len > 0 then Printf.printf "\n"
  with End_of_file -> ()
