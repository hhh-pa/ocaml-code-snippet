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
    let m = int_of_string tokens.(0) in
    let lam = float_of_string tokens.(1) in
    let r = Array.init m (fun i -> float_of_string tokens.(2 + i)) in
    let mu = (Array.fold_left (+.) 0.0 r) /. float_of_int m in
    let var_val = ref 0.0 in
    for i = 0 to m - 1 do
      let d = r.(i) -. mu in
      var_val := !var_val +. d *. d
    done;
    let var = !var_val /. float_of_int (m - 1) in
    let v = ref var in
    for t = 0 to m - 1 do
      v := lam *. !v +. (1.0 -. lam) *. r.(t) *. r.(t)
    done;
    let ewma = !v in
    let bom = ref 0.0 in
    let ba = ref 0.0 in
    let bb = ref 0.0 in
    let bll = ref (-1e300) in
    let bgf = ref var in
    for ai = 1 to 30 do
      let a = float_of_int ai *. 0.01 in
      for bi = 60 to 95 do
        let b = float_of_int bi *. 0.01 in
        if a +. b < 1.0 then begin
          let om = var *. (1.0 -. a -. b) in
          let vv = ref var in
          let ll = ref 0.0 in
          for t = 0 to m - 1 do
            ll := !ll -. 0.5 *. (log !vv +. r.(t) *. r.(t) /. !vv);
            vv := om +. a *. r.(t) *. r.(t) +. b *. !vv
          done;
          if !ll > !bll then begin
            bll := !ll;
            bom := om;
            ba := a;
            bb := b;
            bgf := !vv
          end
        end
      done
    done;
    Printf.printf "EWMA: %.6f\n" (ewma +. 0.0);
    Printf.printf "GARCH: %.6f %.6f %.6f %.6f\n" (!bom +. 0.0) (!ba +. 0.0) (!bb +. 0.0) (!bll +. 0.0);
    Printf.printf "GFILTER: %.6f\n" (!bgf +. 0.0)
  end
