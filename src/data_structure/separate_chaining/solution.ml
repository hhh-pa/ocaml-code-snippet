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
    let t = Array.init m (fun _ -> ref []) in
    let h k =
      let r = Int64.to_int (Int64.rem k (Int64.of_int m)) in
      if r < 0 then r + m else r
    in
    for _ = 0 to q - 1 do
      if !pos < Array.length tokens then begin
        let op = next_int () in
        if op = 1 then begin
          let k = next_int64 () in
          let v = next_int64 () in
          let b = t.(h k) in
          let rec update_list = function
            | [] -> [(k, v)]
            | (k', v') :: rest ->
              if k' = k then (k, v) :: rest
              else (k', v') :: update_list rest
          in
          b := update_list !b
        end else if op = 2 then begin
          let k = next_int64 () in
          let b = !(t.(h k)) in
          let rec find_list = function
            | [] -> None
            | (k', v') :: rest ->
              if k' = k then Some v'
              else find_list rest
          in
          match find_list b with
          | Some v -> Printf.printf "%Ld\n" v
          | None -> print_endline "NONE"
        end else if op = 3 then begin
          let k = next_int64 () in
          let b = t.(h k) in
          let rec del_list = function
            | [] -> (false, [])
            | (k', v') :: rest ->
              if k' = k then (true, rest)
              else
                let (done_f, r) = del_list rest in
                (done_f, (k', v') :: r)
          in
          let (done_f, new_b) = del_list !b in
          b := new_b;
          if done_f then print_endline "OK"
          else print_endline "NONE"
        end else if op = 4 then begin
          for j = 0 to m - 1 do
            if j > 0 then print_string " ";
            print_int (List.length !(t.(j)))
          done;
          print_newline ()
        end
      end
    done
  end
