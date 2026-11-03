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
  if Array.length tokens >= 2 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let next_float () = float_of_string (next_str ()) in
    let ytm = next_float () in
    let m = next_int () in
    let kp = Array.make m 0 in
    let cf = Array.make m 0.0 in
    for i = 0 to m - 1 do
      kp.(i) <- next_int ();
      cf.(i) <- next_float ()
    done;
    let yp = ytm /. 2.0 in
    let df k = (1.0 +. yp) ** (float_of_int (-k)) in
    let p_val = ref 0.0 in
    for i = 0 to m - 1 do
      p_val := !p_val +. cf.(i) *. df kp.(i)
    done;
    let p = !p_val in
    let dper = ref 0.0 in
    for i = 0 to m - 1 do
      dper := !dper +. float_of_int kp.(i) *. cf.(i) *. df kp.(i)
    done;
    let dper = !dper /. p in
    let cper = ref 0.0 in
    for i = 0 to m - 1 do
      cper := !cper +. float_of_int (kp.(i) * (kp.(i) + 1)) *. cf.(i) *. df kp.(i)
    done;
    let cper = !cper /. (p *. (1.0 +. yp) *. (1.0 +. yp)) in
    let mac = dper /. 2.0 in
    let mod_val = mac /. (1.0 +. yp) in
    let conv = cper /. 4.0 in
    let nk = next_int () in
    let keys = Array.init nk (fun _ -> next_int ()) in
    let krd = Array.make nk 0.0 in
    for j = 0 to nk - 1 do
      let pu = ref 0.0 in
      let pd = ref 0.0 in
      for i = 0 to m - 1 do
        let ru = if kp.(i) = keys.(j) then yp +. 0.0001 else yp in
        pu := !pu +. cf.(i) *. ((1.0 +. ru) ** float_of_int (- kp.(i)));
        let rd = if kp.(i) = keys.(j) then yp -. 0.0001 else yp in
        pd := !pd +. cf.(i) *. ((1.0 +. rd) ** float_of_int (- kp.(i)))
      done;
      krd.(j) <- -. (!pu -. !pd) /. (2.0 *. p *. 0.0001) /. 2.0
    done;
    let kl = next_int () in
    let al = next_float () in
    let pl = al *. ((1.0 +. yp) ** float_of_int (-kl)) in
    let dl = float_of_int kl in
    let cl = float_of_int (kl * (kl + 1)) /. ((1.0 +. yp) *. (1.0 +. yp)) in
    let imm = (abs_float (p -. pl) /. pl < 1e-6) && (abs_float (dper -. dl) < 1e-4) && (cper > cl +. 1e-9) in
    Printf.printf "PRICE: %.6f\n" (p +. 0.0);
    Printf.printf "MAC: %.6f\n" (mac +. 0.0);
    Printf.printf "MOD: %.6f\n" (mod_val +. 0.0);
    Printf.printf "CONV: %.6f\n" (conv +. 0.0);
    Printf.printf "KRD:";
    Array.iter (fun x -> Printf.printf " %.6f" (x +. 0.0)) krd;
    Printf.printf "\nIMMUNIZED: %s\n" (if imm then "YES" else "NO")
  end
