let () =
  try
    let algo = Scanf.scanf " %s" (fun x -> x) in
    let n = Scanf.scanf " %d" (fun x -> x) in
    let steps_t = Scanf.scanf " %Ld" (fun x -> x) in
    let region = Array.make n 0 in
    let pc = Array.make n 0 in
    let aux = Array.make n 0 in
    let entries = Array.make n 0L in
    let flag = Array.make n 0 in
    let level = Array.make n 0 in
    let choosing = Array.make n 0 in
    let b = Array.make n 1 in
    let c = Array.make n 1 in
    let number = Array.make n 0L in
    let victim = Array.make (n + 1) 0 in
    let turn = ref 0 in
    let k = ref 0 in
    let step i =
      let j = (i + 1) mod n in
      if region.(i) = 0 then begin
        region.(i) <- 1;
        pc.(i) <- 0;
        aux.(i) <- 0
      end else if region.(i) = 2 then begin
        region.(i) <- 3;
        pc.(i) <- 0
      end else if region.(i) = 3 then begin
        if algo = "peterson2" then begin
          flag.(i) <- 0;
          region.(i) <- 0
        end else if algo = "filter" then begin
          level.(i) <- 0;
          region.(i) <- 0
        end else if algo = "bakery" then begin
          number.(i) <- 0L;
          region.(i) <- 0
        end else if algo = "dijkstra" then begin
          if pc.(i) = 0 then begin
            c.(i) <- 1;
            pc.(i) <- 1
          end else begin
            b.(i) <- 1;
            region.(i) <- 0
          end
        end else if algo = "burns" then begin
          flag.(i) <- 0;
          region.(i) <- 0
        end
      end else begin
        if algo = "peterson2" then begin
          if pc.(i) = 0 then begin
            flag.(i) <- 1;
            pc.(i) <- 1
          end else if pc.(i) = 1 then begin
            turn := j;
            pc.(i) <- 2
          end else begin
            if flag.(j) = 1 && !turn = j then ()
            else begin
              region.(i) <- 2;
              entries.(i) <- Int64.add entries.(i) 1L
            end
          end
        end else if algo = "filter" then begin
          let l_lvl = pc.(i) / 3 in
          let sub = pc.(i) mod 3 in
          if sub = 0 then begin
            level.(i) <- l_lvl + 1;
            pc.(i) <- pc.(i) + 1
          end else if sub = 1 then begin
            victim.(l_lvl + 1) <- i;
            pc.(i) <- pc.(i) + 1
          end else begin
            let blocked = ref false in
            for t = 0 to n - 1 do
              if t <> i && level.(t) >= l_lvl + 1 then
                blocked := true
            done;
            if !blocked && victim.(l_lvl + 1) = i then ()
            else if l_lvl + 1 = n - 1 then begin
              region.(i) <- 2;
              entries.(i) <- Int64.add entries.(i) 1L
            end else
              pc.(i) <- pc.(i) + 1
          end
        end else if algo = "bakery" then begin
          if pc.(i) = 0 then begin
            choosing.(i) <- 1;
            pc.(i) <- 1
          end else if pc.(i) = 1 then begin
            let mx = ref 0L in
            for t = 0 to n - 1 do
              if number.(t) > !mx then mx := number.(t)
            done;
            number.(i) <- Int64.add !mx 1L;
            pc.(i) <- 2
          end else if pc.(i) = 2 then begin
            choosing.(i) <- 0;
            pc.(i) <- 3;
            aux.(i) <- 0
          end else begin
            let t = aux.(i) in
            if t = i then
              aux.(i) <- aux.(i) + 1
            else if t >= n then begin
              region.(i) <- 2;
              entries.(i) <- Int64.add entries.(i) 1L
            end else if choosing.(t) = 1 then ()
            else if number.(t) <> 0L && (number.(t) < number.(i) || (number.(t) = number.(i) && t < i)) then ()
            else
              aux.(i) <- aux.(i) + 1
          end
        end else if algo = "dijkstra" then begin
          if pc.(i) = 0 then begin
            b.(i) <- 0;
            pc.(i) <- 1
          end else if pc.(i) = 1 then begin
            pc.(i) <- (if !k <> i then 2 else 4)
          end else if pc.(i) = 2 then begin
            c.(i) <- 1;
            pc.(i) <- 3
          end else if pc.(i) = 3 then begin
            if b.(!k) = 1 then k := i;
            pc.(i) <- 1
          end else if pc.(i) = 4 then begin
            c.(i) <- 0;
            pc.(i) <- 5
          end else begin
            let fail = ref false in
            for t = 0 to n - 1 do
              if t <> i && c.(t) = 0 then fail := true
            done;
            if !fail then pc.(i) <- 1
            else begin
              region.(i) <- 2;
              entries.(i) <- Int64.add entries.(i) 1L
            end
          end
        end else if algo = "burns" then begin
          if pc.(i) = 0 then begin
            flag.(i) <- 0;
            pc.(i) <- 1
          end else if pc.(i) = 1 then begin
            flag.(i) <- 1;
            pc.(i) <- 2;
            aux.(i) <- 0
          end else if pc.(i) = 2 then begin
            let t = aux.(i) in
            if t >= i then begin
              aux.(i) <- i + 1;
              pc.(i) <- 3
            end else if flag.(t) = 1 then begin
              pc.(i) <- 4
            end else
              aux.(i) <- aux.(i) + 1
          end else if pc.(i) = 4 then begin
            flag.(i) <- 0;
            pc.(i) <- 5
          end else if pc.(i) = 5 then begin
            if flag.(aux.(i)) = 1 then ()
            else begin
              pc.(i) <- 1;
              aux.(i) <- 0
            end
          end else begin
            let t = aux.(i) in
            if t >= n then begin
              region.(i) <- 2;
              entries.(i) <- Int64.add entries.(i) 1L
            end else if flag.(t) = 1 then ()
            else
              aux.(i) <- aux.(i) + 1
          end
        end
      end
    in
    let maxconc = ref 0L in
    let s = ref 0L in
    while !s < steps_t do
      let i = Int64.to_int (Int64.rem !s (Int64.of_int n)) in
      step i;
      let cc = ref 0L in
      for t = 0 to n - 1 do
        if region.(t) = 2 then cc := Int64.add !cc 1L
      done;
      if !cc > !maxconc then maxconc := !cc;
      s := Int64.add !s 1L
    done;
    for i = 0 to n - 1 do
      Printf.printf "P%d: entries=%Ld\n" i entries.(i)
    done;
    Printf.printf "MAXCONCURRENT: %Ld\n" !maxconc;
    Printf.printf "STEPS: %Ld\n" steps_t
  with End_of_file -> ()
