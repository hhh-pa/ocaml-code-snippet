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
  if Array.length tokens >= 1 then begin
    let b = int_of_string tokens.(0) in
    let o_arr = Array.make b 0.0 in
    let h_arr = Array.make b 0.0 in
    let l_arr = Array.make b 0.0 in
    let c_arr = Array.make b 0.0 in
    let v_arr = Array.make b 0.0 in
    let idx = ref 1 in
    for i = 0 to b - 1 do
      o_arr.(i) <- float_of_string tokens.(!idx); incr idx;
      h_arr.(i) <- float_of_string tokens.(!idx); incr idx;
      l_arr.(i) <- float_of_string tokens.(!idx); incr idx;
      c_arr.(i) <- float_of_string tokens.(!idx); incr idx;
      v_arr.(i) <- float_of_string tokens.(!idx); incr idx
    done;
    (* Roll *)
    let roll = ref 0.0 in
    if b >= 3 then begin
      let m1 = ref 0.0 in
      let m2 = ref 0.0 in
      let n = float_of_int (b - 2) in
      for t = 1 to b - 2 do
        m1 := !m1 +. (c_arr.(t) -. c_arr.(t - 1));
        m2 := !m2 +. (c_arr.(t + 1) -. c_arr.(t))
      done;
      let m1 = !m1 /. n in
      let m2 = !m2 /. n in
      let cv = ref 0.0 in
      for t = 1 to b - 2 do
        cv := !cv +. ((c_arr.(t) -. c_arr.(t - 1)) -. m1) *. ((c_arr.(t + 1) -. c_arr.(t)) -. m2)
      done;
      let cv = !cv /. n in
      if cv < 0.0 then roll := 2.0 *. sqrt (-. cv)
    end;
    (* Corwin-Schultz *)
    let cs = ref 0.0 in
    if b >= 2 then begin
      let s_acc = ref 0.0 in
      let cnt = ref 0 in
      let denom_const = 3.0 -. 2.0 *. sqrt 2.0 in
      for t = 0 to b - 2 do
        let b0 = log (h_arr.(t) /. l_arr.(t)) in
        let b1 = log (h_arr.(t + 1) /. l_arr.(t + 1)) in
        let beta = b0 *. b0 +. b1 *. b1 in
        let g = log (max h_arr.(t) h_arr.(t + 1) /. min l_arr.(t) l_arr.(t + 1)) in
        let gamma = g *. g in
        let alpha = (sqrt (2.0 *. beta) -. sqrt beta) /. denom_const -. sqrt (gamma /. denom_const) in
        let st = 2.0 *. (exp alpha -. 1.0) /. (1.0 +. exp alpha) in
        let st = if st < 0.0 then 0.0 else st in
        s_acc := !s_acc +. st;
        incr cnt
      done;
      cs := !s_acc /. float_of_int !cnt
    end;
    (* signs *)
    let sg = Array.make b 1 in
    for t = 1 to b - 1 do
      sg.(t) <-
        if c_arr.(t) > c_arr.(t - 1) then 1
        else if c_arr.(t) < c_arr.(t - 1) then -1
        else sg.(t - 1)
    done;
    (* Kyle lambda *)
    let kyle = ref 0.0 in
    if b >= 2 then begin
      let mx = ref 0.0 in
      let mr = ref 0.0 in
      let n = float_of_int (b - 1) in
      for t = 1 to b - 1 do
        mx := !mx +. float_of_int sg.(t) *. v_arr.(t);
        mr := !mr +. (c_arr.(t) /. c_arr.(t - 1) -. 1.0)
      done;
      let mx = !mx /. n in
      let mr = !mr /. n in
      let cv = ref 0.0 in
      let vv = ref 0.0 in
      for t = 1 to b - 1 do
        let dx = float_of_int sg.(t) *. v_arr.(t) -. mx in
        let dr = (c_arr.(t) /. c_arr.(t - 1) -. 1.0) -. mr in
        cv := !cv +. dx *. dr;
        vv := !vv +. dx *. dx
      done;
      if !vv > 0.0 then kyle := !cv /. !vv
    end;
    (* Amihud *)
    let am = ref 0.0 in
    let an = ref 0 in
    for t = 1 to b - 1 do
      let dv = c_arr.(t) *. v_arr.(t) in
      if dv > 0.0 then begin
        am := !am +. abs_float (c_arr.(t) /. c_arr.(t - 1) -. 1.0) /. dv;
        incr an
      end
    done;
    let amihud = if !an > 0 then !am /. float_of_int !an else 0.0 in
    (* VPIN *)
    let tot = Array.fold_left (+.) 0.0 v_arr in
    let nb = min 50 b in
    let nb = if nb < 1 then 1 else nb in
    let vb = tot /. float_of_int nb in
    let vpin = ref 0.0 in
    let fill = ref 0.0 in
    let buy = ref 0.0 in
    let sell = ref 0.0 in
    for t = 0 to b - 1 do
      let rem = ref v_arr.(t) in
      while !rem > 1e-12 do
        let take = min !rem (vb -. !fill) in
        if sg.(t) > 0 then buy := !buy +. take else sell := !sell +. take;
        fill := !fill +. take;
        rem := !rem -. take;
        if !fill >= vb -. 1e-12 then begin
          vpin := !vpin +. abs_float (!buy -. !sell);
          fill := 0.0;
          buy := 0.0;
          sell := 0.0
        end
      done
    done;
    if !fill > 1e-12 then vpin := !vpin +. abs_float (!buy -. !sell);
    let vpin_val = if tot > 0.0 then !vpin /. tot else 0.0 in
    Printf.printf "ROLL: %.6f\n" (!roll +. 0.0);
    Printf.printf "CS: %.6f\n" (!cs +. 0.0);
    Printf.printf "KYLE: %.6f\n" (!kyle +. 0.0);
    Printf.printf "AMIHUD: %.6f\n" (amihud +. 0.0);
    Printf.printf "VPIN: %.6f\n" (vpin_val +. 0.0)
  end
