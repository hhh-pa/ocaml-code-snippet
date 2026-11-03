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
    let q = next_int () in

    let cap = ref (max 32 (q + 2)) in
    let vals = ref (Array.make !cap 0L) in
    let links = ref (Array.make !cap 0) in
    let count = ref 0 in
    let head = ref (-1) in
    let tail = ref (-1) in

    let alloc x =
      if !count >= !cap then begin
        let new_cap = !cap * 2 in
        let grow_v a =
          let na = Array.make new_cap 0L in
          Array.blit !a 0 na 0 !cap;
          a := na
        in
        let grow_l a =
          let na = Array.make new_cap 0 in
          Array.blit !a 0 na 0 !cap;
          a := na
        in
        grow_v vals;
        grow_l links;
        cap := new_cap
      end;
      let idx = !count in
      incr count;
      (!vals).(idx) <- x;
      (!links).(idx) <- 0;
      idx
    in

    let push_front x =
      let n = alloc x in
      (!links).(n) <- (0 lxor (!head + 1));
      if !head = -1 then begin
        head := n;
        tail := n
      end else begin
        (!links).(!head) <- (!links).(!head) lxor (n + 1);
        head := n
      end
    in

    let push_back x =
      let n = alloc x in
      (!links).(n) <- ((!tail + 1) lxor 0);
      if !tail = -1 then begin
        head := n;
        tail := n
      end else begin
        (!links).(!tail) <- (!links).(!tail) lxor (n + 1);
        tail := n
      end
    in

    let pop_front () =
      if !head = -1 then None
      else begin
        let o = (!vals).(!head) in
        let nxt = (!links).(!head) lxor 0 in
        if nxt = 0 then begin
          head := -1;
          tail := -1
        end else begin
          let ni = nxt - 1 in
          (!links).(ni) <- (!links).(ni) lxor (!head + 1);
          head := ni
        end;
        Some o
      end
    in

    let pop_back () =
      if !tail = -1 then None
      else begin
        let o = (!vals).(!tail) in
        let prv = (!links).(!tail) lxor 0 in
        if prv = 0 then begin
          head := -1;
          tail := -1
        end else begin
          let pi = prv - 1 in
          (!links).(pi) <- (!links).(pi) lxor (!tail + 1);
          tail := pi
        end;
        Some o
      end
    in

    let print_list () =
      let first = ref true in
      let prev = ref (-1) in
      let cur = ref !head in
      while !cur <> -1 do
        if not !first then print_string " ";
        first := false;
        Printf.printf "%Ld" (!vals).(!cur);
        let nxt = (!links).(!cur) lxor (!prev + 1) in
        prev := !cur;
        cur := (if nxt = 0 then -1 else nxt - 1)
      done;
      print_newline ()
    in

    for _ = 0 to q - 1 do
      if !pos < Array.length tokens then begin
        let op = next_int () in
        if op = 1 then begin
          let x = next_int64 () in
          push_front x
        end else if op = 2 then begin
          let x = next_int64 () in
          push_back x
        end else if op = 3 then begin
          match pop_front () with
          | Some v -> Printf.printf "%Ld\n" v
          | None -> print_endline "EMPTY"
        end else if op = 4 then begin
          match pop_back () with
          | Some v -> Printf.printf "%Ld\n" v
          | None -> print_endline "EMPTY"
        end else if op = 5 then begin
          print_list ()
        end
      end
    done
  end
