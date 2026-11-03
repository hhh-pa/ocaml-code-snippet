let inf = 4000000000000000000L

let () =
  try
    let h = Scanf.scanf " %d" (fun x -> x) in
    let w = Scanf.scanf " %d" (fun x -> x) in
    let e = Array.make_matrix h w 0L in
    for r = 0 to h - 1 do
      for c = 0 to w - 1 do
        e.(r).(c) <- Scanf.scanf " %Ld" (fun x -> x)
      done
    done;
    let dp = Array.make_matrix h w inf in
    let par = Array.make_matrix h w 0 in
    for c = 0 to w - 1 do
      dp.(0).(c) <- e.(0).(c)
    done;
    for r = 1 to h - 1 do
      for c = 0 to w - 1 do
        let best = ref inf in
        let bi = ref c in
        let c_start = max 0 (c - 1) in
        let c_end = min (w - 1) (c + 1) in
        for pc = c_start to c_end do
          if dp.(r - 1).(pc) < !best then begin
            best := dp.(r - 1).(pc);
            bi := pc
          end
        done;
        dp.(r).(c) <- Int64.add e.(r).(c) !best;
        par.(r).(c) <- !bi
      done
    done;
    let mc = ref 0 in
    for c = 1 to w - 1 do
      if dp.(h - 1).(c) < dp.(h - 1).(!mc) then mc := c
    done;
    let seam = Array.make h 0 in
    seam.(h - 1) <- !mc;
    for r = h - 1 downto 1 do
      seam.(r - 1) <- par.(r).(seam.(r))
    done;
    Printf.printf "%Ld\n" dp.(h - 1).(!mc);
    for r = 0 to h - 1 do
      if r > 0 then print_string " ";
      print_int seam.(r)
    done;
    print_newline ()
  with End_of_file -> ()
