let () =
  let tokens =
    let buf = Buffer.create 4096 in
    (try while true do Buffer.add_channel buf stdin 4096 done with End_of_file -> ());
    let s = Buffer.contents buf in
    let len = String.length s in
    let res = ref [] in
    let i = ref 0 in
    while !i < len do
      while !i < len && (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\r' || s.[!i] = '\n') do incr i done;
      if !i < len then begin
        let st = !i in
        while !i < len && not (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\r' || s.[!i] = '\n') do incr i done;
        res := String.sub s st (!i - st) :: !res
      end
    done;
    Array.of_list (List.rev !res)
  in
  if Array.length tokens >= 6 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let next_float () = float_of_string (next_str ()) in
    let n = next_int () in
    let v_thresh = next_float () in
    let d_thresh = next_float () in
    let t_thresh = next_float () in
    let r_thresh = next_float () in
    let m = next_int () in
    let px = Array.make m 0.0 in
    let vol = Array.make m 0.0 in
    for i = 0 to m - 1 do
      px.(i) <- next_float ();
      vol.(i) <- next_float ()
    done;
    (* tick *)
    let nt = m / n in
    let tickc = if nt > 0 then px.(nt * n - 1) else px.(0) in
    (* volume *)
    let nv = ref 0 in
    let acc_v = ref 0.0 in
    let volc = ref px.(0) in
    for i = 0 to m - 1 do
      acc_v := !acc_v +. vol.(i);
      if !acc_v >= v_thresh -. 1e-12 then begin
        incr nv;
        acc_v := 0.0;
        volc := px.(i)
      end
    done;
    (* dollar *)
    let nd = ref 0 in
    let acc_d = ref 0.0 in
    let dollc = ref px.(0) in
    for i = 0 to m - 1 do
      acc_d := !acc_d +. px.(i) *. vol.(i);
      if !acc_d >= d_thresh -. 1e-12 then begin
        incr nd;
        acc_d := 0.0;
        dollc := px.(i)
      end
    done;
    (* tick imbalance *)
    let ntib = ref 0 in
    let cum = ref 0.0 in
    let tibc = ref px.(0) in
    let prev = ref 1 in
    for i = 1 to m - 1 do
      let b =
        if px.(i) > px.(i - 1) then 1
        else if px.(i) < px.(i - 1) then -1
        else !prev
      in
      prev := b;
      cum := !cum +. float_of_int b;
      if abs_float !cum >= t_thresh -. 1e-12 then begin
        incr ntib;
        cum := 0.0;
        tibc := px.(i)
      end
    done;
    (* volume run *)
    let nvr = ref 0 in
    let cumv = ref 0.0 in
    let vrc = ref px.(0) in
    prev := 1;
    for i = 1 to m - 1 do
      let b =
        if px.(i) > px.(i - 1) then 1
        else if px.(i) < px.(i - 1) then -1
        else !prev
      in
      prev := b;
      cumv := !cumv +. float_of_int b *. vol.(i);
      if abs_float !cumv >= r_thresh -. 1e-12 then begin
        incr nvr;
        cumv := 0.0;
        vrc := px.(i)
      end
    done;
    Printf.printf "TICK: %d %.2f\n" nt (tickc +. 0.0);
    Printf.printf "VOLUME: %d %.2f\n" !nv (!volc +. 0.0);
    Printf.printf "DOLLAR: %d %.2f\n" !nd (!dollc +. 0.0);
    Printf.printf "TIB: %d %.2f\n" !ntib (!tibc +. 0.0);
    Printf.printf "VRUN: %d %.2f\n" !nvr (!vrc +. 0.0)
  end
