let rec bpm u adj match_r seen =
  let rec loop = function
    | [] -> false
    | v :: rest ->
      if seen.(v) then loop rest
      else begin
        seen.(v) <- true;
        if match_r.(v) < 0 || bpm match_r.(v) adj match_r seen then begin
          match_r.(v) <- u;
          true
        end else
          loop rest
      end
  in
  loop adj.(u)

let () =
  try
    let n, _s = Scanf.scanf " %d %Ld" (fun n s -> (n, s)) in
    let a = Array.make_matrix n n 0L in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        a.(i).(j) <- Scanf.scanf " %Ld" (fun x -> x)
      done
    done;
    let terms = ref [] in
    let rec step () =
      let any = ref false in
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          if a.(i).(j) > 0L then any := true
        done
      done;
      if !any then begin
        let adj = Array.init n (fun i ->
          let l = ref [] in
          for j = n - 1 downto 0 do
            if a.(i).(j) > 0L then l := j :: !l
          done;
          !l
        ) in
        let match_r = Array.make n (-1) in
        for u = 0 to n - 1 do
          let seen = Array.make n false in
          ignore (bpm u adj match_r seen)
        done;
        let ml = Array.make n (-1) in
        for v = 0 to n - 1 do
          if match_r.(v) >= 0 then
            ml.(match_r.(v)) <- v
        done;
        let c = ref (-1L) in
        for i = 0 to n - 1 do
          let v = a.(i).(ml.(i)) in
          if !c < 0L || v < !c then c := v
        done;
        for i = 0 to n - 1 do
          a.(i).(ml.(i)) <- Int64.sub a.(i).(ml.(i)) !c
        done;
        terms := (!c, ml) :: !terms;
        step ()
      end
    in
    step ();
    let terms = List.rev !terms in
    Printf.printf "%d\n" (List.length terms);
    List.iter (fun (c, ml) ->
      Printf.printf "%Ld" c;
      for i = 0 to n - 1 do
        Printf.printf " %d" ml.(i)
      done;
      Printf.printf "\n"
    ) terms
  with End_of_file -> ()
