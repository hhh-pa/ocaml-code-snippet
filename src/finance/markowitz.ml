let solve a_orig b_orig =
  let n = Array.length a_orig in
  let a = Array.init n (fun i -> Array.copy a_orig.(i)) in
  let b = Array.copy b_orig in
  for k = 0 to n - 1 do
    let piv = ref k in
    let best = ref (abs_float a.(k).(k)) in
    for i = k + 1 to n - 1 do
      if abs_float a.(i).(k) > !best then begin
        best := abs_float a.(i).(k);
        piv := i
      end
    done;
    let tmp_row = a.(k) in a.(k) <- a.(!piv); a.(!piv) <- tmp_row;
    let tmp_b = b.(k) in b.(k) <- b.(!piv); b.(!piv) <- tmp_b;
    for i = k + 1 to n - 1 do
      let f = a.(i).(k) /. a.(k).(k) in
      for j = k to n - 1 do
        a.(i).(j) <- a.(i).(j) -. f *. a.(k).(j)
      done;
      b.(i) <- b.(i) -. f *. b.(k)
    done
  done;
  let x = Array.make n 0.0 in
  for k = n - 1 downto 0 do
    let s = ref b.(k) in
    for j = k + 1 to n - 1 do
      s := !s -. a.(k).(j) *. x.(j)
    done;
    x.(k) <- !s /. a.(k).(k)
  done;
  x

let inverse a_orig =
  let n = Array.length a_orig in
  let a = Array.init n (fun i -> Array.copy a_orig.(i)) in
  let inv = Array.make_matrix n n 0.0 in
  for i = 0 to n - 1 do inv.(i).(i) <- 1.0 done;
  for k = 0 to n - 1 do
    let piv = ref k in
    let best = ref (abs_float a.(k).(k)) in
    for i = k + 1 to n - 1 do
      if abs_float a.(i).(k) > !best then begin
        best := abs_float a.(i).(k);
        piv := i
      end
    done;
    let tmp_a = a.(k) in a.(k) <- a.(!piv); a.(!piv) <- tmp_a;
    let tmp_inv = inv.(k) in inv.(k) <- inv.(!piv); inv.(!piv) <- tmp_inv;
    let d = a.(k).(k) in
    for j = 0 to n - 1 do
      a.(k).(j) <- a.(k).(j) /. d;
      inv.(k).(j) <- inv.(k).(j) /. d
    done;
    for i = 0 to n - 1 do
      if i <> k then begin
        let f = a.(i).(k) in
        for j = 0 to n - 1 do
          a.(i).(j) <- a.(i).(j) -. f *. a.(k).(j);
          inv.(i).(j) <- inv.(i).(j) -. f *. inv.(k).(j)
        done
      end
    done
  done;
  inv

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
  if Array.length tokens >= 1 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let next_float () = float_of_string (next_str ()) in
    let n = next_int () in
    let mu = Array.init n (fun _ -> next_float ()) in
    let s_mat = Array.make_matrix n n 0.0 in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        s_mat.(i).(j) <- next_float ()
      done
    done;
    let rf = next_float () in
    let tg = Array.init 3 (fun _ -> next_float ()) in
    let tau = next_float () in
    let p_vec = Array.init n (fun _ -> next_float ()) in
    let q_val = next_float () in
    let om = next_float () in
    (* min variance *)
    let a_mv = Array.make_matrix (n + 1) (n + 1) 0.0 in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        a_mv.(i).(j) <- s_mat.(i).(j)
      done
    done;
    for i = 0 to n - 1 do
      a_mv.(i).(n) <- 1.0;
      a_mv.(n).(i) <- 1.0
    done;
    let b_mv = Array.make (n + 1) 0.0 in
    b_mv.(n) <- 1.0;
    let sol = solve a_mv b_mv in
    (* tangency *)
    let ex = Array.init n (fun i -> mu.(i) -. rf) in
    let tw = solve s_mat ex in
    let ts = Array.fold_left (+.) 0.0 tw in
    for i = 0 to n - 1 do tw.(i) <- tw.(i) /. ts done;
    (* frontier *)
    let fvar = Array.make 3 0.0 in
    for f = 0 to 2 do
      let f_mat = Array.make_matrix (n + 2) (n + 2) 0.0 in
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          f_mat.(i).(j) <- s_mat.(i).(j)
        done
      done;
      for i = 0 to n - 1 do
        f_mat.(i).(n) <- mu.(i);
        f_mat.(n).(i) <- mu.(i);
        f_mat.(i).(n + 1) <- 1.0;
        f_mat.(n + 1).(i) <- 1.0
      done;
      let fb = Array.make (n + 2) 0.0 in
      fb.(n) <- tg.(f);
      fb.(n + 1) <- 1.0;
      let w = solve f_mat fb in
      let v = ref 0.0 in
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          v := !v +. w.(i) *. s_mat.(i).(j) *. w.(j)
        done
      done;
      fvar.(f) <- !v
    done;
    (* Black-Litterman, one view *)
    let ts_mat = Array.make_matrix n n 0.0 in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        ts_mat.(i).(j) <- s_mat.(i).(j) *. tau
      done
    done;
    let its = inverse ts_mat in
    let m_mat = Array.make_matrix n n 0.0 in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        m_mat.(i).(j) <- its.(i).(j) +. p_vec.(i) *. p_vec.(j) /. om
      done
    done;
    let rhs = Array.make n 0.0 in
    for i = 0 to n - 1 do
      let s = ref 0.0 in
      for j = 0 to n - 1 do
        s := !s +. its.(i).(j) *. mu.(j)
      done;
      rhs.(i) <- !s +. p_vec.(i) *. q_val /. om
    done;
    let im_mat = inverse m_mat in
    let bl = Array.make n 0.0 in
    for i = 0 to n - 1 do
      let s = ref 0.0 in
      for j = 0 to n - 1 do
        s := !s +. im_mat.(i).(j) *. rhs.(j)
      done;
      bl.(i) <- !s
    done;
    Printf.printf "MINVAR:";
    for i = 0 to n - 1 do Printf.printf " %.6f" (sol.(i) +. 0.0) done;
    Printf.printf "\nTANGENCY:";
    Array.iter (fun x -> Printf.printf " %.6f" (x +. 0.0)) tw;
    Printf.printf "\nFRONTIER:";
    for f = 0 to 2 do Printf.printf " %.6f,%.6f" (tg.(f) +. 0.0) (fvar.(f) +. 0.0) done;
    Printf.printf "\nBL:";
    Array.iter (fun x -> Printf.printf " %.6f" (x +. 0.0)) bl;
    Printf.printf "\n"
  end
