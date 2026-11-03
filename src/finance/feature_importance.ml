let r2score y p n =
  let mu = (Array.fold_left (+.) 0.0 y) /. float_of_int n in
  let sse = ref 0.0 in
  let sst = ref 0.0 in
  for i = 0 to n - 1 do
    let e = y.(i) -. p.(i) in
    let d = y.(i) -. mu in
    sse := !sse +. e *. e;
    sst := !sst +. d *. d
  done;
  if !sst = 0.0 then (if !sse = 0.0 then 1.0 else 0.0)
  else 1.0 -. !sse /. !sst

let () =
  let tokens =
    let buf = Buffer.create 4096 in
    (try while true do Buffer.add_channel buf stdin 4096 done with End_of_file -> ());
    let s = Buffer.contents buf in
    let len = String.length s in
    let res = ref [] in
    let i = ref 0 in
    while !i < len do
      while !i < len && (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\r' || s.[!i] = '\n') do incr i done;
      if !i < len then begin
        let st = !i in
        while !i < len && not (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\r' || s.[!i] = '\n') do incr i done;
        res := String.sub s st (!i - st) :: !res
      end
    done;
    Array.of_list (List.rev !res)
  in
  if Array.length tokens >= 2 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let next_float () = float_of_string (next_str ()) in
    let n = next_int () in
    let p = next_int () in
    let x_mat = Array.make_matrix n p 0.0 in
    for i = 0 to n - 1 do
      for j = 0 to p - 1 do
        x_mat.(i).(j) <- next_float ()
      done
    done;
    let y = Array.init n (fun _ -> next_float ()) in
    let w = Array.init p (fun _ -> next_float ()) in
    let mu = (Array.fold_left (+.) 0.0 y) /. float_of_int n in
    let tot = ref 0.0 in
    for i = 0 to n - 1 do
      let d = y.(i) -. mu in
      tot := !tot +. d *. d
    done;
    let tot = !tot in
    let mdi = Array.make p 0.0 in
    let mda = Array.make p 0.0 in
    let sfi = Array.make p 0.0 in
    (* MDI *)
    for j = 0 to p - 1 do
      let idx = Array.init n (fun i -> i) in
      Array.sort (fun a b ->
        let va = x_mat.(a).(j) in
        let vb = x_mat.(b).(j) in
        if va < vb then -1
        else if va > vb then 1
        else compare a b
      ) idx;
      let best = ref 0.0 in
      for s = 1 to n - 1 do
        if x_mat.(idx.(s)).(j) <> x_mat.(idx.(s - 1)).(j) then begin
          let nl = float_of_int s in
          let nr = float_of_int (n - s) in
          let ml = ref 0.0 in
          let mr = ref 0.0 in
          for t = 0 to s - 1 do ml := !ml +. y.(idx.(t)) done;
          for t = s to n - 1 do mr := !mr +. y.(idx.(t)) done;
          let ml = !ml /. nl in
          let mr = !mr /. nr in
          let sl = ref 0.0 in
          let sr = ref 0.0 in
          for t = 0 to s - 1 do
            let d = y.(idx.(t)) -. ml in
            sl := !sl +. d *. d
          done;
          for t = s to n - 1 do
            let d = y.(idx.(t)) -. mr in
            sr := !sr +. d *. d
          done;
          let gain = tot -. !sl -. !sr in
          if gain > !best then best := gain
        end
      done;
      mdi.(j) <- !best
    done;
    (* MDA *)
    let base = Array.make n 0.0 in
    for i = 0 to n - 1 do
      let s = ref 0.0 in
      for j = 0 to p - 1 do
        s := !s +. x_mat.(i).(j) *. w.(j)
      done;
      base.(i) <- !s
    done;
    let r2b = r2score y base n in
    for j = 0 to p - 1 do
      let pr = Array.make n 0.0 in
      for i = 0 to n - 1 do
        let s = ref 0.0 in
        for k = 0 to p - 1 do
          let v = if k = j then x_mat.((i + 1) mod n).(k) else x_mat.(i).(k) in
          s := !s +. v *. w.(k)
        done;
        pr.(i) <- !s
      done;
      mda.(j) <- r2b -. r2score y pr n
    done;
    (* SFI *)
    for j = 0 to p - 1 do
      let mx = ref 0.0 in
      for i = 0 to n - 1 do mx := !mx +. x_mat.(i).(j) done;
      let mx = !mx /. float_of_int n in
      let cov = ref 0.0 in
      let vv = ref 0.0 in
      for i = 0 to n - 1 do
        let dx = x_mat.(i).(j) -. mx in
        cov := !cov +. dx *. (y.(i) -. mu);
        vv := !vv +. dx *. dx
      done;
      let slope = if !vv = 0.0 then 0.0 else !cov /. !vv in
      let icept = mu -. slope *. mx in
      let pr = Array.init n (fun i -> icept +. slope *. x_mat.(i).(j)) in
      sfi.(j) <- r2score y pr n
    done;
    Printf.printf "MDI:";
    Array.iter (fun v -> Printf.printf " %.6f" (v +. 0.0)) mdi;
    Printf.printf "\nMDA:";
    Array.iter (fun v -> Printf.printf " %.6f" (v +. 0.0)) mda;
    Printf.printf "\nSFI:";
    Array.iter (fun v -> Printf.printf " %.6f" (v +. 0.0)) sfi;
    Printf.printf "\n"
  end
