let rec bpm u adj match_r seen =
  let rec loop = function
    | [] -> false
    | v :: rest ->
      if seen.(v) then loop rest
      else begin
        seen.(v) <- true;
        if match_r.(v) < 0 || bpm match_r.(v) adj match_r seen then begin
          match_r.(v) <- u;
          true
        end else
          loop rest
      end
  in
  loop adj.(u)

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let e = Scanf.scanf " %d" (fun x -> x) in
    let adj = Array.make n [] in
    for _ = 1 to e do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      adj.(u) <- v :: adj.(u)
    done;
    for u = 0 to n - 1 do
      adj.(u) <- List.sort compare adj.(u)
    done;
    let match_r = Array.make m (-1) in
    for u = 0 to n - 1 do
      let seen = Array.make m false in
      ignore (bpm u adj match_r seen)
    done;
    let match_l = Array.make n (-1) in
    for v = 0 to m - 1 do
      if match_r.(v) >= 0 then
        match_l.(match_r.(v)) <- v
    done;
    let possible = ref true in
    for u = 0 to n - 1 do
      if match_l.(u) < 0 then possible := false
    done;
    if not !possible then
      print_endline "NO"
    else
      for u = 0 to n - 1 do
        Printf.printf "%d %d\n" u match_l.(u)
      done
  with End_of_file -> ()
