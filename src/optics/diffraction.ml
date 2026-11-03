let sn x =
  if abs_float x < 5e-7 then 0.0 else x

let pi = acos (-1.0)

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let mode = Scanf.scanf " %d" (fun x -> x) in
    let a = Array.init n (fun _ -> Scanf.scanf " %f" (fun x -> x)) in
    let lam, z, dx =
      if mode = 1 then
        let l = Scanf.scanf " %f" (fun x -> x) in
        let z_val = Scanf.scanf " %f" (fun x -> x) in
        let d = Scanf.scanf " %f" (fun x -> x) in
        (l, z_val, d)
      else
        (1.0, 1.0, 1.0)
    in
    let re = Array.make n 0.0 in
    let im = Array.make n 0.0 in
    for j = 0 to n - 1 do
      if mode = 0 then begin
        re.(j) <- a.(j);
        im.(j) <- 0.0
      end else begin
        let x = (float_of_int j -. float_of_int n /. 2.0) *. dx in
        let ph = pi *. x *. x /. (lam *. z) in
        re.(j) <- a.(j) *. cos ph;
        im.(j) <- a.(j) *. sin ph
      end
    done;
    let kk = ref 0 in
    for i = 1 to n - 1 do
      let b = ref (n lsr 1) in
      while (!kk land !b) <> 0 do
        kk := !kk land (lnot !b);
        b := !b lsr 1
      done;
      kk := !kk lor !b;
      if i < !kk then begin
        let tr = re.(i) in
        re.(i) <- re.(!kk);
        re.(!kk) <- tr;
        let ti = im.(i) in
        im.(i) <- im.(!kk);
        im.(!kk) <- ti
      end
    done;
    let m = ref 2 in
    while !m <= n do
      let ang = -.2.0 *. pi /. float_of_int !m in
      let wr = cos ang in
      let wi = sin ang in
      let k = ref 0 in
      while !k < n do
        let cr = ref 1.0 in
        let ci = ref 0.0 in
        let half = !m / 2 in
        for j = 0 to half - 1 do
          let idx2 = !k + j + half in
          let idx1 = !k + j in
          let tr = !cr *. re.(idx2) -. !ci *. im.(idx2) in
          let ti = !cr *. im.(idx2) +. !ci *. re.(idx2) in
          re.(idx2) <- re.(idx1) -. tr;
          im.(idx2) <- im.(idx1) -. ti;
          re.(idx1) <- re.(idx1) +. tr;
          im.(idx1) <- im.(idx1) +. ti;
          let nr = !cr *. wr -. !ci *. wi in
          ci := !cr *. wi +. !ci *. wr;
          cr := nr
        done;
        k := !k + !m
      done;
      m := !m * 2
    done;
    let intensity = Array.make n 0.0 in
    let mx = ref 0.0 in
    for k = 0 to n - 1 do
      intensity.(k) <- re.(k) *. re.(k) +. im.(k) *. im.(k);
      if intensity.(k) > !mx then mx := intensity.(k)
    done;
    print_string "I:";
    for k = 0 to n - 1 do
      let v = if !mx = 0.0 then 0.0 else intensity.(k) /. !mx in
      Printf.printf " %.6f" (sn v)
    done;
    print_newline ()
  with End_of_file -> ()
