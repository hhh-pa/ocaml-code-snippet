let inf = 4_000_000_000_000_000_000L

let rec gcd a b =
  if b = 0L then a else gcd b (Int64.rem a b)

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let es = Array.init m (fun _ ->
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      let w = Scanf.scanf " %Ld" (fun x -> x) in
      (u, v, w)
    ) in
    let f = Array.make_matrix (n + 1) n inf in
    for v = 0 to n - 1 do
      f.(0).(v) <- 0L
    done;
    for k = 1 to n do
      let f_prev = f.(k - 1) in
      let f_curr = f.(k) in
      Array.iter (fun (u, v, w) ->
        if f_prev.(u) < inf then begin
          let cand = Int64.add f_prev.(u) w in
          if cand < f_curr.(v) then f_curr.(v) <- cand
        end
      ) es
    done;
    let half_inf = Int64.div inf 2L in
    let any_found = ref false in
    let bn = ref 0L in
    let bd = ref 1L in
    for v = 0 to n - 1 do
      if f.(n).(v) < half_inf then begin
        let av = ref false in
        let vn = ref 0L in
        let vd = ref 1L in
        for k = 0 to n - 1 do
          if f.(k).(v) < half_inf then begin
            let num = Int64.sub f.(n).(v) f.(k).(v) in
            let den = Int64.of_int (n - k) in
            if not !av || (
              (* num / den > vn / vd  <=>  num * vd > vn * den *)
              let lhs = Int64.to_float num /. Int64.to_float den in
              let rhs = Int64.to_float !vn /. Int64.to_float !vd in
              lhs > rhs
            ) then begin
              vn := num;
              vd := den;
              av := true
            end
          end
        done;
        if !av then begin
          if not !any_found || (
            let lhs = Int64.to_float !vn /. Int64.to_float !vd in
            let rhs = Int64.to_float !bn /. Int64.to_float !bd in
            lhs < rhs
          ) then begin
            bn := !vn;
            bd := !vd;
            any_found := true
          end
        end
      end
    done;
    if not !any_found then
      Printf.printf "NO CYCLE\n"
    else begin
      let g = gcd (Int64.abs !bn) (Int64.abs !bd) in
      let res_n = ref (Int64.div !bn g) in
      let res_d = ref (Int64.div !bd g) in
      if !res_d < 0L then begin
        res_n := Int64.neg !res_n;
        res_d := Int64.neg !res_d
      end;
      Printf.printf "%Ld/%Ld\n" !res_n !res_d
    end
  with End_of_file -> ()
