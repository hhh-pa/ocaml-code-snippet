(* Probability of backtest overfitting (AFML ch 11). *)

let read_all () =
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_char buf (input_char stdin)
     done
   with End_of_file -> ());
  Buffer.contents buf

let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let split_ws s =
  let toks = ref [] in
  let cur = Buffer.create 16 in
  let flush () =
    if Buffer.length cur > 0 then begin
      toks := Buffer.contents cur :: !toks;
      Buffer.clear cur
    end
  in
  String.iter
    (fun c -> if is_space c then flush () else Buffer.add_char cur c)
    s;
  flush ();
  List.rev !toks

let sharpe vals =
  let n = List.length vals in
  if n < 2 then 0.0
  else
    let mean = List.fold_left ( +. ) 0.0 vals /. float_of_int n in
    let tot =
      List.fold_left
        (fun a v ->
          let d = v -. mean in
          a +. (d *. d))
        0.0 vals
    in
    let std = sqrt (tot /. float_of_int (n - 1)) in
    if std = 0.0 then 0.0 else mean /. std

let midranks xs =
  let n = List.length xs in
  let arr = Array.of_list xs in
  let order = Array.init n (fun i -> i) in
  Array.sort (fun a b -> Float.compare arr.(a) arr.(b)) order;
  let ranks = Array.make n 0.0 in
  let i = ref 0 in
  while !i < n do
    let j = ref !i in
    while !j + 1 < n && arr.(order.(!j + 1)) = arr.(order.(!i)) do
      incr j
    done;
    let r = float_of_int (!i + !j + 2) /. 2.0 in
    for k = !i to !j do
      ranks.(order.(k)) <- r
    done;
    i := !j + 1
  done;
  Array.to_list ranks

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let arr = Array.of_list toks in
      let t = int_of_string arr.(0) in
      let n = int_of_string arr.(1) in
      let s = int_of_string arr.(2) in
      let mat =
        Array.init t (fun i ->
            Array.init n (fun j -> float_of_string arr.(3 + (i * n) + j)))
      in
      let q = t / s in
      let rem = t mod s in
      let bounds = Array.make (s + 1) 0 in
      for g = 0 to s - 1 do
        bounds.(g + 1) <- (bounds.(g) + q + if g < rem then 1 else 0)
      done;
      let kk = s / 2 in
      let combo = Array.init kk (fun i -> i) in
      let bad = ref 0 in
      let total = ref 0 in
      let med = float_of_int (n + 1) /. 2.0 in
      let stop = ref false in
      while not !stop do
        let inset = Array.make s false in
        for i = 0 to kk - 1 do
          inset.(combo.(i)) <- true
        done;
        let is_rows = ref [] in
        let oos_rows = ref [] in
        for g = 0 to s - 1 do
          for r = bounds.(g) to bounds.(g + 1) - 1 do
            if inset.(g) then is_rows := r :: !is_rows
            else oos_rows := r :: !oos_rows
          done
        done;
        let is_rows = List.rev !is_rows in
        let oos_rows = List.rev !oos_rows in
        let is_sh =
          List.init n (fun c ->
              sharpe (List.map (fun r -> mat.(r).(c)) is_rows))
        in
        let oos_sh =
          List.init n (fun c ->
              sharpe (List.map (fun r -> mat.(r).(c)) oos_rows))
        in
        let is_rk = midranks is_sh in
        let oos_rk = midranks oos_sh in
        let sel_sum = ref 0.0 in
        let sel_n = ref 0 in
        List.iter2
          (fun ir otr ->
            if ir > med then begin
              sel_sum := !sel_sum +. otr;
              incr sel_n
            end)
          is_rk oos_rk;
        let w =
          if !sel_n = 0 then 0.5
          else !sel_sum /. float_of_int !sel_n /. float_of_int (n + 1)
        in
        if w < 0.5 then incr bad;
        incr total;
        let p = ref (kk - 1) in
        while !p >= 0 && combo.(!p) = s - kk + !p do
          decr p
        done;
        if !p < 0 then stop := true
        else begin
          combo.(!p) <- combo.(!p) + 1;
          for i = !p + 1 to kk - 1 do
            combo.(i) <- combo.(i - 1) + 1
          done
        end
      done;
      Printf.printf "PBO %.6f" (float_of_int !bad /. float_of_int !total)
