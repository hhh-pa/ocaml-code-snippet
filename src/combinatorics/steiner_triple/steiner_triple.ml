module PairSet = Set.Make(struct
  type t = int * int
  let compare = compare
end)

let rec bt v need trips by_pair covered chosen =
  if List.length !chosen = need then true
  else begin
    let fa = ref (-1) in
    let fb = ref (-1) in
    for a = 0 to v - 1 do
      if !fa < 0 then
        for b = a + 1 to v - 1 do
          if !fa < 0 && not covered.(a).(b) then begin
            fa := a;
            fb := b
          end
        done
    done;
    let rec try_ti = function
      | [] -> false
      | ti :: rest ->
        let (ta, tb, tc) = trips.(ti) in
        if covered.(ta).(tb) || covered.(ta).(tc) || covered.(tb).(tc) then
          try_ti rest
        else begin
          covered.(ta).(tb) <- true;
          covered.(ta).(tc) <- true;
          covered.(tb).(tc) <- true;
          chosen := (ta, tb, tc) :: !chosen;
          if bt v need trips by_pair covered chosen then true
          else begin
            chosen := List.tl !chosen;
            covered.(ta).(tb) <- false;
            covered.(ta).(tc) <- false;
            covered.(tb).(tc) <- false;
            try_ti rest
          end
        end
    in
    try_ti by_pair.(!fa).(!fb)
  end

let rec kbt idx total per classes triples =
  if idx = total then
    Array.for_all (fun c -> List.length !c = per) classes
  else
    let (ta, tb, tc) = triples.(idx) in
    let rec try_class c_idx =
      if c_idx = Array.length classes then false
      else
        let c = classes.(c_idx) in
        if List.length !c >= per then
          try_class (c_idx + 1)
        else begin
          let used = Array.make 32 false in
          List.iter (fun (x, y, z) ->
            used.(x) <- true;
            used.(y) <- true;
            used.(z) <- true
          ) !c;
          if used.(ta) || used.(tb) || used.(tc) then
            try_class (c_idx + 1)
          else begin
            c := (ta, tb, tc) :: !c;
            if kbt (idx + 1) total per classes triples then true
            else begin
              c := List.tl !c;
              if !c = [] then false
              else try_class (c_idx + 1)
            end
          end
        end
    in
    try_class 0

let () =
  try
    let mode = Scanf.scanf " %s" (fun s -> s) in
    if mode = "S" || mode = "K" then begin
      let v = Scanf.scanf " %d" (fun x -> x) in
      let trips_list = ref [] in
      let by_pair = Array.make_matrix v v [] in
      for a = 0 to v - 1 do
        for b = a + 1 to v - 1 do
          for c = b + 1 to v - 1 do
            let ti = List.length !trips_list in
            trips_list := (a, b, c) :: !trips_list;
            by_pair.(a).(b) <- ti :: by_pair.(a).(b);
            by_pair.(a).(c) <- ti :: by_pair.(a).(c);
            by_pair.(b).(c) <- ti :: by_pair.(b).(c)
          done
        done
      done;
      let trips = Array.of_list (List.rev !trips_list) in
      for a = 0 to v - 1 do
        for b = 0 to v - 1 do
          by_pair.(a).(b) <- List.rev by_pair.(a).(b)
        done
      done;
      let covered = Array.make_matrix v v false in
      let chosen = ref [] in
      ignore (bt v (v * (v - 1) / 6) trips by_pair covered chosen);
      let chosen_sorted = List.sort compare !chosen in
      if mode = "S" then begin
        List.iter (fun (a, b, c) ->
          Printf.printf "%d %d %d\n" a b c
        ) chosen_sorted
      end else begin
        let per = v / 3 in
        let total = List.length chosen_sorted in
        let nc = (total + per - 1) / per in
        let classes = Array.init nc (fun _ -> ref []) in
        let triples_arr = Array.of_list chosen_sorted in
        ignore (kbt 0 total per classes triples_arr);
        Array.iter (fun c ->
          let sorted_c = List.sort compare !c in
          List.iteri (fun i (a, b, c) ->
            if i > 0 then print_char ' ';
            Printf.printf "%d %d %d" a b c
          ) sorted_c;
          print_newline ()
        ) classes
      end
    end else begin
      let v = Scanf.scanf " %d" (fun x -> x) in
      let t = Scanf.scanf " %d" (fun x -> x) in
      let valid = ref (t = v * (v - 1) / 6) in
      let seen = ref PairSet.empty in
      for _ = 1 to t do
        let a = Scanf.scanf " %d" (fun x -> x) in
        let b = Scanf.scanf " %d" (fun x -> x) in
        let c = Scanf.scanf " %d" (fun x -> x) in
        if not (0 <= a && a < b && b < c && c < v) then valid := false;
        let pairs = [(a, b); (a, c); (b, c)] in
        List.iter (fun p ->
          if PairSet.mem p !seen then valid := false;
          seen := PairSet.add p !seen
        ) pairs
      done;
      if PairSet.cardinal !seen <> v * (v - 1) / 2 then valid := false;
      if !valid then print_endline "VALID" else print_endline "INVALID"
    end
  with End_of_file -> ()
