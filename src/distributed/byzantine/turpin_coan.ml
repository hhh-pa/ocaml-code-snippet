module Int64Map = Map.Make(Int64)

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let f = Scanf.scanf " %d" (fun x -> x) in
    let v = Array.init n (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
    let k = Scanf.scanf " %d" (fun x -> x) in
    let faulty = Array.make n false in
    for _ = 0 to k - 1 do
      let x = Scanf.scanf " %d" (fun x -> x) in
      faulty.(x) <- true
    done;
    let w = Array.make n 0L in
    let has = Array.make n false in
    let b = Array.make n 0 in
    for i = 0 to n - 1 do
      if not faulty.(i) then begin
        let cnt = ref Int64Map.empty in
        for j = 0 to n - 1 do
          if not faulty.(j) then
            let val_j = v.(j) in
            let c = match Int64Map.find_opt val_j !cnt with Some x -> x | None -> 0 in
            cnt := Int64Map.add val_j (c + 1) !cnt
        done;
        let best = ref 0L in
        let bc = ref 0 in
        Int64Map.iter (fun key count ->
          if count > !bc then begin
            bc := count;
            best := key
          end
        ) !cnt;
        if !bc >= n - f then begin
          w.(i) <- !best;
          has.(i) <- true;
          b.(i) <- 1
        end
      end
    done;
    let band = ref 1 in
    for i = 0 to n - 1 do
      if not faulty.(i) && b.(i) = 0 then
        band := 0
    done;
    let dec = ref 0L in
    let have = ref false in
    if !band = 1 then begin
      let rec find_dec i =
        if i < n then begin
          if not faulty.(i) && has.(i) then begin
            dec := w.(i);
            have := true
          end else
            find_dec (i + 1)
        end
      in
      find_dec 0
    end;
    for i = 0 to n - 1 do
      if faulty.(i) then
        Printf.printf "P%d: FAULTY\n" i
      else begin
        let w_str = if has.(i) then Int64.to_string w.(i) else "NONE" in
        Printf.printf "P%d: w=%s b=%d\n" i w_str b.(i)
      end
    done;
    Printf.printf "BINARY: %d\n" !band;
    Printf.printf "DECISION: ";
    if !band = 1 && !have then
      Printf.printf "%Ld\n" !dec
    else
      Printf.printf "ABSTAIN\n"
  with End_of_file -> ()
