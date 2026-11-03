let inf = 4000000000000000000L

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
  if Array.length tokens >= 3 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let next_int64 () = Int64.of_string (next_str ()) in
    let m = next_int () in
    let n = next_int () in
    let q = next_int () in
    let a = Array.make_matrix m n inf in

    let insert x =
      if a.(m - 1).(n - 1) <> inf then false
      else begin
        let i = ref (m - 1) in
        let j = ref (n - 1) in
        a.(!i).(!j) <- x;
        let stop = ref false in
        while (!i > 0 || !j > 0) && not !stop do
          let bi = ref !i in
          let bj = ref !j in
          if !i > 0 && a.(!i - 1).(!j) > a.(!bi).(!bj) then begin
            bi := !i - 1;
            bj := !j
          end;
          if !j > 0 && a.(!i).(!j - 1) > a.(!bi).(!bj) then begin
            bi := !i;
            bj := !j - 1
          end;
          if !bi = !i && !bj = !j then stop := true
          else begin
            let t = a.(!i).(!j) in
            a.(!i).(!j) <- a.(!bi).(!bj);
            a.(!bi).(!bj) <- t;
            i := !bi;
            j := !bj
          end
        done;
        true
      end
    in

    let extract_min () =
      if a.(0).(0) = inf then None
      else begin
        let o = a.(0).(0) in
        let i = ref 0 in
        let j = ref 0 in
        a.(0).(0) <- inf;
        let stop = ref false in
        while not !stop do
          let si = ref !i in
          let sj = ref !j in
          if !i + 1 < m && a.(!i + 1).(!j) < a.(!si).(!sj) then begin
            si := !i + 1;
            sj := !j
          end;
          if !j + 1 < n && a.(!i).(!j + 1) < a.(!si).(!sj) then begin
            si := !i;
            sj := !j + 1
          end;
          if !si = !i && !sj = !j then stop := true
          else begin
            a.(!i).(!j) <- a.(!si).(!sj);
            a.(!si).(!sj) <- inf;
            i := !si;
            j := !sj
          end
        done;
        Some o
      end
    in

    let search x =
      let i = ref 0 in
      let j = ref (n - 1) in
      let found = ref false in
      while !i < m && !j >= 0 && not !found do
        if a.(!i).(!j) = x then found := true
        else if a.(!i).(!j) > x then decr j
        else incr i
      done;
      !found
    in

    for _ = 0 to q - 1 do
      if !pos < Array.length tokens then begin
        let op = next_int () in
        if op = 1 then begin
          let x = next_int64 () in
          if not (insert x) then print_endline "FULL"
        end else if op = 2 then begin
          match extract_min () with
          | Some v -> Printf.printf "%Ld\n" v
          | None -> print_endline "EMPTY"
        end else if op = 3 then begin
          let x = next_int64 () in
          print_endline (if search x then "YES" else "NO")
        end
      end
    done
  end
