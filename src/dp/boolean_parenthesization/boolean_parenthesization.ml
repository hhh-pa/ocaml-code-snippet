let () =
  try
    let syms = Scanf.scanf " %s" (fun s -> s) in
    let n = String.length syms in
    let ops =
      if n > 1 then Scanf.scanf " %s" (fun s -> s)
      else ""
    in
    let t = Array.make_matrix n n 0L in
    let f = Array.make_matrix n n 0L in
    for i = 0 to n - 1 do
      if syms.[i] = 'T' then t.(i).(i) <- 1L
      else f.(i).(i) <- 1L
    done;
    for l = 2 to n do
      for i = 0 to n - l do
        let j = i + l - 1 in
        let cur_t = ref 0L in
        let cur_f = ref 0L in
        for k = i to j - 1 do
          let op = ops.[k] in
          let lt = t.(i).(k) in
          let lf = f.(i).(k) in
          let rt = t.(k + 1).(j) in
          let rf = f.(k + 1).(j) in
          let tot = Int64.mul (Int64.add lt lf) (Int64.add rt rf) in
          let tt =
            if op = '&' then Int64.mul lt rt
            else if op = '|' then Int64.sub tot (Int64.mul lf rf)
            else Int64.add (Int64.mul lt rf) (Int64.mul lf rt)
          in
          cur_t := Int64.add !cur_t tt;
          cur_f := Int64.add !cur_f (Int64.sub tot tt)
        done;
        t.(i).(j) <- !cur_t;
        f.(i).(j) <- !cur_f
      done
    done;
    Printf.printf "%Ld\n" t.(0).(n - 1)
  with End_of_file -> ()
