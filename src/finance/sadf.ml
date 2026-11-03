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
    let m = int_of_string tokens.(0) in
    let r0 = float_of_string tokens.(1) in
    let y = Array.init m (fun i -> float_of_string tokens.(2 + i)) in
    let emin = max 3 (int_of_float (r0 *. float_of_int m) - 1) in
    let sadf = ref (-1e100) in
    let arg = ref emin in
    for e = emin to m - 1 do
      let nobs = float_of_int e in
      let my = ref 0.0 in
      let md = ref 0.0 in
      for t = 1 to e do
        my := !my +. y.(t - 1);
        md := !md +. (y.(t) -. y.(t - 1))
      done;
      my := !my /. nobs;
      md := !md /. nobs;
      let sxx = ref 0.0 in
      let sxy = ref 0.0 in
      for t = 1 to e do
        let dx = y.(t - 1) -. !my in
        let dy = (y.(t) -. y.(t - 1)) -. !md in
        sxx := !sxx +. dx *. dx;
        sxy := !sxy +. dx *. dy
      done;
      let tst = ref 0.0 in
      if !sxx > 0.0 && e > 2 then begin
        let b = !sxy /. !sxx in
        let a = !md -. b *. !my in
        let sse = ref 0.0 in
        for t = 1 to e do
          let r = (y.(t) -. y.(t - 1)) -. a -. b *. y.(t - 1) in
          sse := !sse +. r *. r
        done;
        let s2 = !sse /. (nobs -. 2.0) in
        if s2 > 0.0 then tst := b /. sqrt (s2 /. !sxx)
      end;
      if !tst > !sadf then begin
        sadf := !tst;
        arg := e
      end
    done;
    Printf.printf "SADF: %.6f\n" !sadf;
    Printf.printf "ARGMAX: %d\n" !arg
  end
