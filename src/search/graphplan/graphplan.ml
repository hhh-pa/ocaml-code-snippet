let bits_of p_cnt m =
  let r = ref [] in
  for p = 0 to p_cnt - 1 do
    if (m land (1 lsl p)) <> 0 then r := p :: !r
  done;
  List.rev !r

let solve_case () =
  let p_cnt = Scanf.scanf " %d" (fun x -> x) in
  let a_cnt = Scanf.scanf " %d" (fun x -> x) in
  let n_cnt = a_cnt + p_cnt in
  let pre = Array.make n_cnt 0 in
  let add = Array.make n_cnt 0 in
  let del_ = Array.make n_cnt 0 in

  for i = 0 to a_cnt - 1 do
    let np = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to np - 1 do
      let x = Scanf.scanf " %d" (fun v -> v) in
      pre.(i) <- pre.(i) lor (1 lsl x)
    done;
    let na = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to na - 1 do
      let x = Scanf.scanf " %d" (fun v -> v) in
      add.(i) <- add.(i) lor (1 lsl x)
    done;
    let nd = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to nd - 1 do
      let x = Scanf.scanf " %d" (fun v -> v) in
      del_.(i) <- del_.(i) lor (1 lsl x)
    done
  done;

  for p = 0 to p_cnt - 1 do
    pre.(a_cnt + p) <- 1 lsl p;
    add.(a_cnt + p) <- 1 lsl p;
    del_.(a_cnt + p) <- 0
  done;

  let ni = Scanf.scanf " %d" (fun x -> x) in
  let initmask = ref 0 in
  for _ = 0 to ni - 1 do
    let x = Scanf.scanf " %d" (fun v -> v) in
    initmask := !initmask lor (1 lsl x)
  done;

  let ng = Scanf.scanf " %d" (fun x -> x) in
  let goalmask = ref 0 in
  for _ = 0 to ng - 1 do
    let x = Scanf.scanf " %d" (fun v -> v) in
    goalmask := !goalmask lor (1 lsl x)
  done;

  let actids = ref [] in
  let amut = ref [] in
  let pmodes = ref [!initmask] in
  let pmut = ref [Array.make_matrix p_cnt p_cnt false] in
  let nogood = Hashtbl.create 128 in

  let get_pmodes lvl = List.nth !pmodes lvl in
  let get_pmut lvl = List.nth !pmut lvl in
  let get_actids lvl = List.nth !actids lvl in
  let get_amut lvl = List.nth !amut lvl in

  let amutex_at lvl a b =
    let (u, v) = if a > b then (b, a) else (a, b) in
    (get_amut lvl).(u).(v)
  in

  let achievers pr lvl =
    let r = ref [] in
    List.iter (fun a ->
      if (add.(a) land (1 lsl pr)) <> 0 then r := a :: !r
    ) (get_actids lvl);
    List.rev !r
  in

  let rec extract k goals =
    if k = 0 then begin
      let ok = List.for_all (fun g -> (!initmask land (1 lsl g)) <> 0) goals in
      (ok, [])
    end else begin
      let gm = ref 0 in
      List.iter (fun g -> gm := !gm lor (1 lsl g)) goals;
      let key = (k, !gm) in
      if Hashtbl.mem nogood key then (false, [])
      else begin
        let (ok, plan) = cover k goals [] 0 in
        if not ok then Hashtbl.add nogood key ();
        (ok, plan)
      end
    end

  and cover k unc chosen nextmask =
    match unc with
    | [] ->
        let (ok, sub) = extract (k - 1) (bits_of p_cnt nextmask) in
        if not ok then (false, [])
        else (true, sub @ [chosen])
    | g :: _ ->
        let achs = achievers g (k - 1) in
        let rec try_ach = function
          | [] -> (false, [])
          | a :: rest ->
              let mx = List.exists (fun c -> amutex_at (k - 1) a c) chosen in
              if mx then try_ach rest
              else begin
                let nu = List.filter (fun x -> (add.(a) land (1 lsl x)) = 0) unc in
                let nc = chosen @ [a] in
                let (ok, plan) = cover k nu nc (nextmask lor pre.(a)) in
                if ok then (true, plan)
                else try_ach rest
              end
        in
        try_ach achs
  in

  let solved = ref false in
  let plan = ref [] in
  let solved_l = ref 0 in

  let l = ref 0 in
  while !l <= 12 && not !solved do
    let cur_l = !l in
    let cur_pmode = get_pmodes cur_l in
    let cur_pmut = get_pmut cur_l in
    if (cur_pmode land !goalmask) = !goalmask then begin
      let gs = bits_of p_cnt !goalmask in
      let mok = ref true in
      let gs_arr = Array.of_list gs in
      let len_gs = Array.length gs_arr in
      for i = 0 to len_gs - 1 do
        for j = i + 1 to len_gs - 1 do
          if cur_pmut.(gs_arr.(i)).(gs_arr.(j)) then mok := false
        done
      done;
      if !mok then begin
        let (ok, pl) = extract cur_l gs in
        if ok then begin
          solved := true;
          plan := pl;
          solved_l := cur_l
        end
      end
    end;

    if not !solved && cur_l < 12 then begin
      let aids = ref [] in
      for a = 0 to n_cnt - 1 do
        if (pre.(a) land cur_pmode) = pre.(a) then aids := a :: !aids
      done;
      let aids_list = List.rev !aids in
      let aids_arr = Array.of_list aids_list in
      let len_aids = Array.length aids_arr in
      let am = Array.make_matrix n_cnt n_cnt false in
      for i = 0 to len_aids - 1 do
        let x = aids_arr.(i) in
        for j = i + 1 to len_aids - 1 do
          let y = aids_arr.(j) in
          let mx = ref false in
          if ((add.(x) land del_.(y)) <> 0) || ((add.(y) land del_.(x)) <> 0) then
            mx := true
          else if ((del_.(x) land pre.(y)) <> 0) || ((del_.(y) land pre.(x)) <> 0) then
            mx := true
          else begin
            let pa = bits_of p_cnt pre.(x) in
            let pb = bits_of p_cnt pre.(y) in
            List.iter (fun u ->
              List.iter (fun v ->
                if cur_pmut.(u).(v) then mx := true
              ) pb
            ) pa
          end;
          am.(x).(y) <- !mx;
          am.(y).(x) <- !mx
        done
      done;

      actids := !actids @ [aids_list];
      amut := !amut @ [am];

      let nmask = ref 0 in
      List.iter (fun a -> nmask := !nmask lor add.(a)) aids_list;

      let pm = Array.make_matrix p_cnt p_cnt false in
      let mc = ref 0 in
      for x = 0 to p_cnt - 1 do
        if (!nmask land (1 lsl x)) <> 0 then begin
          for y = x + 1 to p_cnt - 1 do
            if (!nmask land (1 lsl y)) <> 0 then begin
              let mx = ref true in
              List.iter (fun u ->
                if (add.(u) land (1 lsl x)) <> 0 then begin
                  List.iter (fun v ->
                    if (add.(v) land (1 lsl y)) <> 0 then begin
                      if u = v || not am.(u).(v) then mx := false
                    end
                  ) aids_list
                end
              ) aids_list;
              pm.(x).(y) <- !mx;
              pm.(y).(x) <- !mx;
              if !mx then incr mc
            end
          done
        end
      done;

      let prevmc = ref 0 in
      for x = 0 to p_cnt - 1 do
        for y = x + 1 to p_cnt - 1 do
          if cur_pmut.(x).(y) then incr prevmc
        done
      done;
      let same = (!nmask = cur_pmode && !mc = !prevmc) in
      pmodes := !pmodes @ [!nmask];
      pmut := !pmut @ [pm];
      if same then l := 12
    end;
    incr l
  done;

  if not !solved then
    Printf.printf "GRAPHPLAN FAIL\n"
  else begin
    Printf.printf "GRAPHPLAN %d\n" !solved_l;
    List.iter (fun step ->
      let ra = ref [] in
      List.iter (fun a -> if a < a_cnt then ra := a :: !ra) step;
      let sorted_ra = List.sort compare !ra in
      let first = ref true in
      List.iter (fun a ->
        if not !first then Printf.printf " ";
        Printf.printf "%d" a;
        first := false
      ) sorted_ra;
      Printf.printf "\n"
    ) !plan
  end

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
