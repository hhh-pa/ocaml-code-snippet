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
  if Array.length tokens >= 1 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let next_int64 () = Int64.of_string (next_str ()) in
    let n = next_int () in
    let l_arr = Array.make n 0L in
    let r_arr = Array.make n 0L in
    let all_u = ref [] in
    for i = 0 to n - 1 do
      l_arr.(i) <- next_int64 ();
      r_arr.(i) <- next_int64 ();
      all_u := l_arr.(i) :: r_arr.(i) :: !all_u
    done;
    let u_sorted = Array.of_list !all_u in
    Array.sort Int64.compare u_sorted;
    let u_uniq = ref [] in
    for i = 0 to Array.length u_sorted - 1 do
      match !u_uniq with
      | prev :: _ when prev = u_sorted.(i) -> ()
      | _ -> u_uniq := u_sorted.(i) :: !u_uniq
    done;
    let u = Array.of_list (List.rev !u_uniq) in
    let m = Array.length u in
    let st_n = 2 * m + 1 in
    let t_tree = Array.init (4 * st_n + 4) (fun _ -> ref []) in

    let rec insert v l r ql qr id =
      if ql >= r || qr <= l then ()
      else if ql <= l && r <= qr then
        t_tree.(v) := id :: !(t_tree.(v))
      else begin
        let mid = (l + r) / 2 in
        insert (2 * v) l mid ql qr id;
        insert (2 * v + 1) mid r ql qr id
      end
    in

    let rec query v l r idx out =
      List.iter (fun id -> out := id :: !out) !(t_tree.(v));
      if r - l > 1 then begin
        let mid = (l + r) / 2 in
        if idx < mid then query (2 * v) l mid idx out
        else query (2 * v + 1) mid r idx out
      end
    in

    let lower_bound target =
      let lo = ref 0 in
      let hi = ref m in
      while !lo < !hi do
        let mid = (!lo + !hi) / 2 in
        if u.(mid) < target then lo := mid + 1
        else hi := mid
      done;
      !lo
    in

    for i = 0 to n - 1 do
      let li = lower_bound l_arr.(i) in
      let ri = lower_bound r_arr.(i) in
      insert 1 0 st_n (2 * li + 1) (2 * ri + 2) i
    done;

    if !pos < Array.length tokens then begin
      let q = next_int () in
      for _ = 0 to q - 1 do
        if !pos < Array.length tokens then begin
          let x = next_int64 () in
          let ei =
            if m = 0 then 0
            else if x < u.(0) then 0
            else if x > u.(m - 1) then 2 * m
            else begin
              let lo = ref 0 in
              let hi = ref m in
              while !lo < !hi do
                let mid = (!lo + !hi) / 2 in
                if u.(mid) <= x then lo := mid + 1
                else hi := mid
              done;
              let k = !lo - 1 in
              if u.(k) = x then 2 * k + 1 else 2 * k + 2
            end
          in
          let found = ref [] in
          query 1 0 st_n ei found;
          let found_sorted = List.sort compare !found in
          Printf.printf "%d\n" (List.length found_sorted);
          if found_sorted <> [] then begin
            let strs = List.map string_of_int found_sorted in
            print_endline (String.concat " " strs)
          end
        end
      done
    end
  end
