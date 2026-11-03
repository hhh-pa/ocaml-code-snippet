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
    let arr = Array.init n (fun _ -> next_int64 ()) in
    let tree = Array.make (4 * n + 4) 0L in
    let lazy_arr = Array.make (4 * n + 4) 0L in

    let rec build node l r =
      if l = r then tree.(node) <- arr.(l)
      else begin
        let mid = (l + r) / 2 in
        let lc = 2 * node in
        let rc = lc + 1 in
        build lc l mid;
        build rc (mid + 1) r;
        tree.(node) <- Int64.add tree.(lc) tree.(rc)
      end
    in
    if n > 0 then build 1 0 (n - 1);

    let push_down node l r =
      let v = lazy_arr.(node) in
      if v <> 0L then begin
        let mid = (l + r) / 2 in
        let lc = 2 * node in
        let rc = lc + 1 in
        let left_len = Int64.of_int (mid - l + 1) in
        let right_len = Int64.of_int (r - mid) in
        tree.(lc) <- Int64.add tree.(lc) (Int64.mul v left_len);
        lazy_arr.(lc) <- Int64.add lazy_arr.(lc) v;
        tree.(rc) <- Int64.add tree.(rc) (Int64.mul v right_len);
        lazy_arr.(rc) <- Int64.add lazy_arr.(rc) v;
        lazy_arr.(node) <- 0L
      end
    in

    let rec update node l r ql qr v =
      if ql <= l && r <= qr then begin
        let span = Int64.of_int (r - l + 1) in
        tree.(node) <- Int64.add tree.(node) (Int64.mul v span);
        lazy_arr.(node) <- Int64.add lazy_arr.(node) v
      end else begin
        push_down node l r;
        let mid = (l + r) / 2 in
        let lc = 2 * node in
        let rc = lc + 1 in
        if ql <= mid then update lc l mid ql qr v;
        if qr > mid then update rc (mid + 1) r ql qr v;
        tree.(node) <- Int64.add tree.(lc) tree.(rc)
      end
    in

    let rec query node l r ql qr =
      if ql <= l && r <= qr then tree.(node)
      else begin
        push_down node l r;
        let mid = (l + r) / 2 in
        let lc = 2 * node in
        let rc = lc + 1 in
        let sum = ref 0L in
        if ql <= mid then sum := Int64.add !sum (query lc l mid ql qr);
        if qr > mid then sum := Int64.add !sum (query rc (mid + 1) r ql qr);
        !sum
      end
    in

    for _ = 0 to q - 1 do
      if !pos < Array.length tokens then begin
        let op = next_str () in
        if op = "UPDATE" then begin
          let l = next_int () in
          let r = next_int () in
          let v = next_int64 () in
          update 1 0 (n - 1) (l - 1) (r - 1) v
        end else if op = "QUERY" then begin
          let l = next_int () in
          let r = next_int () in
          Printf.printf "%Ld\n" (query 1 0 (n - 1) (l - 1) (r - 1))
        end
      end
    done
  end
