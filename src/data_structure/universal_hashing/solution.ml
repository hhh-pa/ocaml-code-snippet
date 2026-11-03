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
  if Array.length tokens >= 4 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let next_int64 () = Int64.of_string (next_str ()) in
    let a = next_int64 () in
    let w = next_int () in
    let k = next_int () in
    let n = next_int () in
    let mask =
      if w >= 64 then -1L
      else if w = 0 then 0L
      else Int64.sub (Int64.shift_left 1L w) 1L
    in
    let shift = w - k in
    for _ = 0 to n - 1 do
      if !pos < Array.length tokens then begin
        let x = next_int64 () in
        let p = Int64.logand (Int64.mul a x) mask in
        let h =
          if shift >= 64 then 0L
          else Int64.shift_right_logical p shift
        in
        Printf.printf "%Ld\n" h
      end
    done
  end
