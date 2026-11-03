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
  if Array.length tokens >= 8 then begin
    let s_val = float_of_string tokens.(0) in
    let k = float_of_string tokens.(1) in
    let t = float_of_string tokens.(2) in
    let r = float_of_string tokens.(3) in
    let sig_val = float_of_string tokens.(4) in
    let n = int_of_string tokens.(5) in
    let opt_type = tokens.(6) in
    let opt_style = tokens.(7) in
    let dt = t /. float_of_int n in
    let u = exp (sig_val *. sqrt dt) in
    let d = 1.0 /. u in
    let disc = exp (-. r *. dt) in
    let p = (exp (r *. dt) -. d) /. (u -. d) in
    let p = max 0.0 (min 1.0 p) in
    let vals = Array.make (n + 1) 0.0 in
    for j = 0 to n do
      let st = s_val *. (u ** float_of_int j) *. (d ** float_of_int (n - j)) in
      vals.(j) <- if opt_type = "call" then max (st -. k) 0.0 else max (k -. st) 0.0
    done;
    for step = n - 1 downto 0 do
      for j = 0 to step do
        let hold = disc *. (p *. vals.(j + 1) +. (1.0 -. p) *. vals.(j)) in
        if opt_style = "american" then begin
          let st = s_val *. (u ** float_of_int j) *. (d ** float_of_int (step - j)) in
          let intrinsic = if opt_type = "call" then max (st -. k) 0.0 else max (k -. st) 0.0 in
          vals.(j) <- max hold intrinsic
        end else
          vals.(j) <- hold
      done
    done;
    Printf.printf "%.4f\n" (vals.(0) +. 0.0)
  end
