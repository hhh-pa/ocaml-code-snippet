let read_all_tokens () =
  let content =
    let b = Buffer.create 4096 in
    let buf = Bytes.create 4096 in
    let rec loop () =
      let n = input stdin buf 0 4096 in
      if n > 0 then (
        Buffer.add_subbytes b buf 0 n;
        loop ()
      )
    in
    loop ();
    Buffer.contents b
  in
  let len = String.length content in
  let tokens = ref [] in
  let i = ref 0 in
  while !i < len do
    while !i < len && (let c = content.[!i] in c = ' ' || c = '\t' || c = '\r' || c = '\n') do
      incr i
    done;
    if !i < len then (
      let start = !i in
      while !i < len && (let c = content.[!i] in c <> ' ' && c <> '\t' && c <> '\r' && c <> '\n') do
        incr i
      done;
      tokens := String.sub content start (!i - start) :: !tokens
    )
  done;
  Array.of_list (List.rev !tokens)

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 2 then (
    let h_cap = int_of_string tokens.(0) in
    let q = int_of_string tokens.(1) in
    let idx = ref 2 in

    let sizes = ref [] in
    let isfree = ref [] in
    if h_cap >= 16 then (
      sizes := [h_cap];
      isfree := [true]
    );
    let alloc = Hashtbl.create 64 in

    for _ = 0 to q - 1 do
      let op = int_of_string tokens.(!idx) in
      incr idx;
      if op = 1 then (
        let s = int_of_string tokens.(!idx) in
        incr idx;
        if s < 1 then print_endline "FAIL"
        else (
          let need = s + 8 in
          let s_arr = Array.of_list !sizes in
          let f_arr = Array.of_list !isfree in
          let len = Array.length s_arr in
          let pick = ref (-1) in
          let k = ref 0 in
          while !k < len && !pick = -1 do
            if f_arr.(!k) && s_arr.(!k) >= need then
              pick := !k;
            incr k
          done;
          if !pick = -1 then print_endline "FAIL"
          else (
            let p = !pick in
            let leftover = s_arr.(p) - need in
            let b = ref 0 in
            for i = 0 to p - 1 do
              b := !b + s_arr.(i)
            done;
            let new_s = ref [] in
            let new_f = ref [] in
            for i = 0 to len - 1 do
              if i = p then (
                if leftover >= 16 then (
                  new_s := leftover :: need :: !new_s;
                  new_f := true :: false :: !new_f
                ) else (
                  new_s := s_arr.(i) :: !new_s;
                  new_f := false :: !new_f
                )
              ) else (
                new_s := s_arr.(i) :: !new_s;
                new_f := f_arr.(i) :: !new_f
              )
            done;
            sizes := List.rev !new_s;
            isfree := List.rev !new_f;
            let allocated_size = if leftover >= 16 then need else s_arr.(p) in
            let payload = !b + 4 in
            Hashtbl.replace alloc payload allocated_size;
            Printf.printf "%d\n" payload
          )
        )
      ) else if op = 2 then (
        let p = int_of_string tokens.(!idx) in
        incr idx;
        if not (Hashtbl.mem alloc p) then print_endline "FAIL"
        else (
          Hashtbl.remove alloc p;
          let b = p - 4 in
          let s_arr = Array.of_list !sizes in
          let f_arr = Array.of_list !isfree in
          let len = Array.length s_arr in
          let k = ref 0 in
          let acc = ref 0 in
          while !k < len && !acc <> b do
            acc := !acc + s_arr.(!k);
            incr k
          done;
          let target = !k in
          f_arr.(target) <- true;
          (* merge with right if free *)
          let right_merged =
            if target + 1 < len && f_arr.(target + 1) then (
              s_arr.(target) <- s_arr.(target) + s_arr.(target + 1);
              true
            ) else false
          in
          (* merge with left if free *)
          let left_merged =
            if target > 0 && f_arr.(target - 1) then (
              s_arr.(target - 1) <- s_arr.(target - 1) + s_arr.(target);
              true
            ) else false
          in
          let new_s = ref [] in
          let new_f = ref [] in
          for i = 0 to len - 1 do
            if (right_merged && i = target + 1) || (left_merged && i = target) then
              ()
            else (
              new_s := s_arr.(i) :: !new_s;
              new_f := f_arr.(i) :: !new_f
            )
          done;
          sizes := List.rev !new_s;
          isfree := List.rev !new_f;
          print_endline "OK"
        )
      ) else if op = 3 then (
        let s_arr = Array.of_list !sizes in
        let f_arr = Array.of_list !isfree in
        let len = Array.length s_arr in
        let strs = ref [] in
        for i = 0 to len - 1 do
          let tag = if f_arr.(i) then "F" else "A" in
          strs := Printf.sprintf "%s%d" tag s_arr.(i) :: !strs
        done;
        print_endline (String.concat " " (List.rev !strs))
      )
    done
  )
