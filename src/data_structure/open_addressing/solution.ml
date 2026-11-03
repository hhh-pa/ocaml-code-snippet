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
    let m = next_int () in
    let q = next_int () in
    let keys = Array.make m 0L in
    let vals = Array.make m 0L in
    let st = Array.make m 0 in (* 0 empty, 1 occupied, 2 tombstone *)
    let h k =
      let r = Int64.to_int (Int64.rem k (Int64.of_int m)) in
      if r < 0 then r + m else r
    in
    for _ = 0 to q - 1 do
      let op = next_int () in
      if op = 1 then begin
        let k = next_int64 () in
        let v = next_int64 () in
        let start = h k in
        let tomb = ref (-1) in
        let found = ref (-1) in
        let t = ref 0 in
        let stop = ref false in
        while !t < m && not !stop do
          let s_idx = (start + !t) mod m in
          if st.(s_idx) = 1 && keys.(s_idx) = k then begin
            found := s_idx;
            stop := true
          end else begin
            if st.(s_idx) = 2 && !tomb = -1 then tomb := s_idx;
            if st.(s_idx) = 0 then begin
              if !tomb = -1 then tomb := s_idx;
              stop := true
            end
          end;
          incr t
        done;
        if !found <> -1 then vals.(!found) <- v
        else if !tomb <> -1 then begin
          keys.(!tomb) <- k;
          vals.(!tomb) <- v;
          st.(!tomb) <- 1
        end else print_endline "FULL"
      end else if op = 2 then begin
        let k = next_int64 () in
        let start = h k in
        let found = ref false in
        let ans = ref 0L in
        let t = ref 0 in
        let stop = ref false in
        while !t < m && not !stop do
          let s_idx = (start + !t) mod m in
          if st.(s_idx) = 0 then stop := true
          else if st.(s_idx) = 1 && keys.(s_idx) = k then begin
            ans := vals.(s_idx);
            found := true;
            stop := true
          end;
          incr t
        done;
        if !found then Printf.printf "%Ld\n" !ans
        else print_endline "NONE"
      end else if op = 3 then begin
        let k = next_int64 () in
        let start = h k in
        let done_flag = ref false in
        let t = ref 0 in
        let stop = ref false in
        while !t < m && not !stop do
          let s_idx = (start + !t) mod m in
          if st.(s_idx) = 0 then stop := true
          else if st.(s_idx) = 1 && keys.(s_idx) = k then begin
            st.(s_idx) <- 2;
            done_flag := true;
            stop := true
          end;
          incr t
        done;
        if !done_flag then print_endline "OK"
        else print_endline "NONE"
      end else if op = 4 then begin
        let b = Buffer.create m in
        for s_idx = 0 to m - 1 do
          Buffer.add_char b (if st.(s_idx) = 0 then '.' else if st.(s_idx) = 1 then 'O' else 'X')
        done;
        print_endline (Buffer.contents b)
      end
    done
  end
