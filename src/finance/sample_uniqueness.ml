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
    let l = next_int () in
    let k = next_int () in
    let t0 = Array.make l 0 in
    let t1 = Array.make l 0 in
    let tmax = ref 0 in
    for i = 0 to l - 1 do
      t0.(i) <- next_int ();
      t1.(i) <- next_int ();
      if t1.(i) > !tmax then tmax := t1.(i)
    done;
    let c = Array.make (!tmax + 1) 0 in
    for i = 0 to l - 1 do
      for t = t0.(i) to t1.(i) do
        c.(t) <- c.(t) + 1
      done
    done;
    let u = Array.make l 0.0 in
    for i = 0 to l - 1 do
      let s = ref 0.0 in
      for t = t0.(i) to t1.(i) do
        s := !s +. 1.0 /. float_of_int c.(t)
      done;
      u.(i) <- !s /. float_of_int (t1.(i) - t0.(i) + 1)
    done;
    print_string "UNIQ:";
    for i = 0 to l - 1 do
      Printf.printf " %.6f" u.(i)
    done;
    print_newline ();
    let lcg = ref 999L in
    let a_const = 6364136223846793005L in
    let c_const = 1442695040888963407L in
    let uu = Array.copy u in
    let draws = Array.make k 0 in
    for d = 0 to k - 1 do
      let tot = ref 0.0 in
      for i = 0 to l - 1 do
        tot := !tot +. uu.(i)
      done;
      lcg := Int64.add (Int64.mul !lcg a_const) c_const;
      let shifted = Int64.add (Int64.shift_right_logical !lcg 11) 1L in
      let pick =
        if !tot <= 0.0 then
          Int64.to_int (Int64.rem shifted (Int64.of_int l))
        else begin
          let r = (Int64.to_float shifted /. 9007199254740992.0) *. !tot in
          let acc = ref 0.0 in
          let p = ref (l - 1) in
          let found = ref false in
          let i = ref 0 in
          while !i < l && not !found do
            acc := !acc +. uu.(!i);
            if !acc > r then begin
              p := !i;
              found := true
            end;
            incr i
          done;
          !p
        end
      in
      draws.(d) <- pick;
      for t = t0.(pick) to t1.(pick) do
        let term = (1.0 /. float_of_int c.(t)) in
        for j = 0 to l - 1 do
          if t0.(j) <= t && t <= t1.(j) then begin
            uu.(j) <- uu.(j) -. term /. float_of_int (t1.(j) - t0.(j) + 1);
            if uu.(j) < 0.0 then uu.(j) <- 0.0
          end
        done
      done
    done;
    print_string "BOOT:";
    for d = 0 to k - 1 do
      Printf.printf " %d" draws.(d)
    done;
    print_newline ()
  end
