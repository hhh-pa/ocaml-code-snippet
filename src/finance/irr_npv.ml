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
    let rate = next_float () in
    let m = next_int () in
    let cf = Array.init m (fun _ -> next_float ()) in
    let npv r =
      let s = ref 0.0 in
      for t = 0 to m - 1 do
        s := !s +. cf.(t) *. ((1.0 +. r) ** float_of_int (-t))
      done;
      !s
    in
    let n_val = npv rate in
    let lo = ref (-0.9999) in
    let hi = ref 10.0 in
    let flo = ref (npv !lo) in
    let fhi = ref (npv !hi) in
    let has = (!flo = 0.0) || (!fhi = 0.0) || ((!flo < 0.0) <> (!fhi < 0.0)) in
    let irr = ref 0.0 in
    if has then begin
      if !flo = 0.0 then irr := !lo
      else if !fhi = 0.0 then irr := !hi
      else begin
        for _it = 0 to 199 do
          let mid = (!lo +. !hi) /. 2.0 in
          let fm = npv mid in
          if fm = 0.0 then begin
            lo := mid;
            hi := mid
          end else if (fm < 0.0) <> (!flo < 0.0) then begin
            hi := mid;
            fhi := fm
          end else begin
            lo := mid;
            flo := fm
          end
        done;
        irr := (!lo +. !hi) /. 2.0
      end
    end;
    let tax = next_float () in
    let we = next_float () in
    let re = next_float () in
    let wd = next_float () in
    let rd = next_float () in
    let wacc = we *. re +. wd *. rd *. (1.0 -. tax) in
    Printf.printf "NPV: %.6f\n" (n_val +. 0.0);
    if has then
      Printf.printf "IRR: %.6f\n" (!irr +. 0.0)
    else
      Printf.printf "IRR: NONE\n";
    Printf.printf "WACC: %.6f\n" (wacc +. 0.0)
  end
