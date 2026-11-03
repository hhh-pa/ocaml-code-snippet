let rec gcd64 a b =
  if b = 0L then a
  else gcd64 b (Int64.rem a b)

let () =
  let tokens =
    let buf = Buffer.create 4096 in
    (try while true do Buffer.add_channel buf stdin 4096 done with End_of_file -> ());
    let s = Buffer.contents buf in
    let len = String.length s in
    let res = ref [] in
    let i = ref 0 in
    while !i < len do
      while !i < len && (let c = s.[!i] in c = ' ' || c = '\t' || c = '\r' || c = '\n') do incr i done;
      if !i < len then begin
        let st = !i in
        while !i < len && (let c = s.[!i] in c <> ' ' && c <> '\t' && c <> '\r' && c <> '\n') do incr i done;
        res := String.sub s st (!i - st) :: !res
      end
    done;
    Array.of_list (List.rev !res)
  in
  if Array.length tokens >= 2 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let next_int64 () = Int64.of_string (next_str ()) in
    let n = next_int () in
    let q = next_int () in
    let a = Array.init n (fun _ -> next_int64 ()) in

    let lg = Array.make (n + 1) 0 in
    for i = 2 to n do
      lg.(i) <- lg.(i / 2) + 1
    done;
    let max_k = lg.(n) + 1 in

    let st_min = Array.make_matrix max_k n 0L in
    let st_max = Array.make_matrix max_k n 0L in
    let st_gcd = Array.make_matrix max_k n 0L in

    for i = 0 to n - 1 do
      st_min.(0).(i) <- a.(i);
      st_max.(0).(i) <- a.(i);
      st_gcd.(0).(i) <- a.(i)
    done;

    for j = 1 to max_k - 1 do
      let len = 1 lsl j in
      let half = 1 lsl (j - 1) in
      let i = ref 0 in
      while !i + len <= n do
        st_min.(j).(!i) <- min st_min.(j - 1).(!i) st_min.(j - 1).(!i + half);
        st_max.(j).(!i) <- max st_max.(j - 1).(!i) st_max.(j - 1).(!i + half);
        st_gcd.(j).(!i) <- gcd64 st_gcd.(j - 1).(!i) st_gcd.(j - 1).(!i + half);
        incr i
      done
    done;

    for _ = 0 to q - 1 do
      if !pos < Array.length tokens then begin
        let t = next_int () in
        let l = next_int () in
        let r = next_int () in
        let span = r - l + 1 in
        let k = lg.(span) in
        let right_start = r + 1 - (1 lsl k) in
        if t = 1 then
          Printf.printf "%Ld\n" (min st_min.(k).(l) st_min.(k).(right_start))
        else if t = 2 then
          Printf.printf "%Ld\n" (max st_max.(k).(l) st_max.(k).(right_start))
        else if t = 3 then
          Printf.printf "%Ld\n" (gcd64 st_gcd.(k).(l) st_gcd.(k).(right_start))
      end
    done
  end
