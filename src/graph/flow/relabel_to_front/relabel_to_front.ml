let inf = 1_000_000_000_000_000_000L

let move_to_front p l =
  let rec extract idx acc = function
    | [] -> l
    | x :: xs ->
        if idx = p then x :: List.rev_append acc xs
        else extract (idx + 1) (x :: acc) xs
  in
  extract 0 [] l

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let s = Scanf.scanf " %d" (fun x -> x) in
    let t = Scanf.scanf " %d" (fun x -> x) in

    let res = Array.make_matrix n n 0L in
    for _ = 0 to m - 1 do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      let c = Scanf.scanf " %Ld" (fun x -> x) in
      res.(u).(v) <- Int64.add res.(u).(v) c
    done;

    let h = Array.make n 0 in
    let e = Array.make n 0L in
    h.(s) <- n;

    for v = 0 to n - 1 do
      if res.(s).(v) > 0L then begin
        let c = res.(s).(v) in
        e.(v) <- c;
        e.(s) <- Int64.sub e.(s) c;
        res.(v).(s) <- Int64.add res.(v).(s) c;
        res.(s).(v) <- 0L
      end
    done;

    let initial_l = ref [] in
    for v = n - 1 downto 0 do
      if v <> s && v <> t then
        initial_l := v :: !initial_l
    done;
    let l_ref = ref !initial_l in
    let cur = Array.make n 0 in
    let p = ref 0 in
    let l_len = List.length !l_ref in

    while !p < l_len do
      let u = List.nth !l_ref !p in
      let old = h.(u) in
      while e.(u) > 0L do
        let v = cur.(u) in
        if v = n then begin
          let mn = ref 1_000_000_000 in
          for w = 0 to n - 1 do
            if res.(u).(w) > 0L then begin
              if h.(w) < !mn then mn := h.(w)
            end
          done;
          h.(u) <- !mn + 1;
          cur.(u) <- 0
        end else if res.(u).(v) > 0L && h.(u) = h.(v) + 1 then begin
          let d = min e.(u) res.(u).(v) in
          res.(u).(v) <- Int64.sub res.(u).(v) d;
          res.(v).(u) <- Int64.add res.(v).(u) d;
          e.(u) <- Int64.sub e.(u) d;
          e.(v) <- Int64.add e.(v) d
        end else
          cur.(u) <- cur.(u) + 1
      done;

      if h.(u) > old then begin
        l_ref := move_to_front !p !l_ref;
        p := 0
      end else
        incr p
    done;

    Printf.printf "%Ld\n" e.(t)
  with End_of_file -> ()
