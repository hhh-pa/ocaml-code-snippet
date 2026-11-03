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
    let n = next_int () in
    let q = next_int () in
    let size = ref 1 in
    while !size < n do
      size := !size * 2
    done;
    let s_val = !size in
    let inf = 2000000000 in
    let tree = Array.make (2 * s_val) inf in
    for i = 0 to n - 1 do
      tree.(s_val + i) <- next_int ()
    done;
    for i = s_val - 1 downto 1 do
      tree.(i) <- min tree.(2 * i) tree.(2 * i + 1)
    done;
    for _ = 0 to q - 1 do
      if !pos < Array.length tokens then begin
        let op = next_str () in
        let x = next_int () in
        let y = next_int () in
        if op = "q" then begin
          let l = ref (x - 1 + s_val) in
          let r = ref (y + s_val) in
          let res = ref inf in
          while !l < !r do
            if (!l land 1) = 1 then begin
              if tree.(!l) < !res then res := tree.(!l);
              incr l
            end;
            if (!r land 1) = 1 then begin
              decr r;
              if tree.(!r) < !res then res := tree.(!r)
            end;
            l := !l / 2;
            r := !r / 2
          done;
          Printf.printf "%d\n" !res
        end else if op = "u" then begin
          let p = ref (x - 1 + s_val) in
          tree.(!p) <- y;
          p := !p / 2;
          while !p > 0 do
            tree.(!p) <- min tree.(2 * !p) tree.(2 * !p + 1);
            p := !p / 2
          done
        end
      end
    done
  end
