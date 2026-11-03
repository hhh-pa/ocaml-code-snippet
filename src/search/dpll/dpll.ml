let solve_case () =
  let v_cnt = Scanf.scanf " %d" (fun x -> x) in
  let c_cnt = Scanf.scanf " %d" (fun x -> x) in
  let clauses = Array.make c_cnt [] in
  for i = 0 to c_cnt - 1 do
    let k = Scanf.scanf " %d" (fun x -> x) in
    let lits = ref [] in
    for _ = 0 to k - 1 do
      lits := Scanf.scanf " %d" (fun x -> x) :: !lits
    done;
    clauses.(i) <- List.rev !lits
  done;

  let rec dpll asn =
    let rec simplify () =
      let changed = ref false in
      let conflict = ref false in
      let ci = ref 0 in
      while !ci < c_cnt && not !conflict do
        let c = clauses.(!ci) in
        let un = ref 0 in
        let ulit = ref 0 in
        let sat = ref false in
        List.iter (fun l ->
          let x = abs l - 1 in
          let ax = asn.(x) in
          if ax = -1 then begin
            incr un;
            ulit := l
          end else if (ax = 1) = (l > 0) then
            sat := true
        ) c;
        if not !sat then begin
          if !un = 0 then conflict := true
          else if !un = 1 then begin
            let x = abs !ulit - 1 in
            let v = if !ulit > 0 then 1 else 0 in
            if asn.(x) = -1 then begin
              asn.(x) <- v;
              changed := true
            end else if asn.(x) <> v then
              conflict := true
          end
        end;
        incr ci
      done;

      if !conflict then false
      else begin
        let pos = Array.make v_cnt false in
        let neg = Array.make v_cnt false in
        for i = 0 to c_cnt - 1 do
          let c = clauses.(i) in
          let sat = List.exists (fun l ->
            let x = abs l - 1 in
            asn.(x) <> -1 && (asn.(x) = 1) = (l > 0)
          ) c in
          if not sat then begin
            List.iter (fun l ->
              let x = abs l - 1 in
              if asn.(x) = -1 then begin
                if l > 0 then pos.(x) <- true
                else neg.(x) <- true
              end
            ) c
          end
        done;
        for x = 0 to v_cnt - 1 do
          if asn.(x) = -1 then begin
            if pos.(x) && not neg.(x) then begin
              asn.(x) <- 1;
              changed := true
            end else if neg.(x) && not pos.(x) then begin
              asn.(x) <- 0;
              changed := true
            end
          end
        done;

        if !changed then begin
          let has_conflict = ref false in
          for i = 0 to c_cnt - 1 do
            let c = clauses.(i) in
            let sat = ref false in
            let un = ref 0 in
            List.iter (fun l ->
              let x = abs l - 1 in
              let ax = asn.(x) in
              if ax = -1 then incr un
              else if (ax = 1) = (l > 0) then sat := true
            ) c;
            if not !sat && !un = 0 then has_conflict := true
          done;
          if !has_conflict then false
          else simplify ()
        end else true
      end
    in

    if not (simplify ()) then false
    else begin
      let has_conflict = ref false in
      for i = 0 to c_cnt - 1 do
        let c = clauses.(i) in
        let sat = ref false in
        let un = ref 0 in
        List.iter (fun l ->
          let x = abs l - 1 in
          let ax = asn.(x) in
          if ax = -1 then incr un
          else if (ax = 1) = (l > 0) then sat := true
        ) c;
        if not !sat && !un = 0 then has_conflict := true
      done;
      if !has_conflict then false
      else begin
        let br = ref (-1) in
        let x = ref 0 in
        while !x < v_cnt && !br = -1 do
          if asn.(!x) = -1 then br := !x;
          incr x
        done;
        if !br < 0 then true
        else begin
          let var = !br in
          let a2 = Array.copy asn in
          a2.(var) <- 1;
          if dpll a2 then begin
            Array.blit a2 0 asn 0 v_cnt;
            true
          end else begin
            let a3 = Array.copy asn in
            a3.(var) <- 0;
            if dpll a3 then begin
              Array.blit a3 0 asn 0 v_cnt;
              true
            end else false
          end
        end
      end
    end
  in

  let asn = Array.make v_cnt (-1) in
  if not (dpll asn) then
    Printf.printf "DPLL UNSAT\n"
  else begin
    Printf.printf "DPLL SAT";
    for x = 0 to v_cnt - 1 do
      Printf.printf " %d" (if asn.(x) = -1 then 0 else asn.(x))
    done;
    Printf.printf "\n"
  end

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
