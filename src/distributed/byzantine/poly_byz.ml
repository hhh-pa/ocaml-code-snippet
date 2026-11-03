module IntMap = Map.Make(Int)

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let f = Scanf.scanf " %d" (fun x -> x) in
    let init = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    let nf = Scanf.scanf " %d" (fun x -> x) in
    let faulty = Array.make n false in
    for _ = 0 to nf - 1 do
      let id = Scanf.scanf " %d" (fun x -> x) in
      faulty.(id) <- true
    done;
    let val_arr = Array.copy init in
    for ph = 0 to f do
      let king = ph mod n in
      let nval = Array.copy val_arr in
      for i = 0 to n - 1 do
        if not faulty.(i) then begin
          let cnt = ref IntMap.empty in
          for j = 0 to n - 1 do
            if not faulty.(j) then
              let v = val_arr.(j) in
              let c = match IntMap.find_opt v !cnt with Some x -> x | None -> 0 in
              cnt := IntMap.add v (c + 1) !cnt
          done;
          let maj = ref (-1) in
          let mc = ref 0 in
          IntMap.iter (fun k v ->
            if v > !mc then begin
              mc := v;
              maj := k
            end
          ) !cnt;
          if !mc >= n - f then
            nval.(i) <- !maj
          else if not faulty.(king) then
            nval.(i) <- val_arr.(king)
        end
      done;
      Array.blit nval 0 val_arr 0 n
    done;
    for i = 0 to n - 1 do
      if not faulty.(i) then
        Printf.printf "P%d: %d\n" i val_arr.(i)
    done
  with End_of_file -> ()
