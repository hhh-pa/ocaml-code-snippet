open Complex

let sn x =
  let v = x +. 0.0 in
  if abs_float v < 5e-7 then 0.0 else v

let pi = acos (-1.0)

let mmul a b =
  let r = Array.make_matrix 2 2 zero in
  for i = 0 to 1 do
    for j = 0 to 1 do
      let s = ref zero in
      for k = 0 to 1 do
        s := add !s (mul a.(i).(k) b.(k).(j))
      done;
      r.(i).(j) <- !s
    done
  done;
  r

let m4mul a b =
  let r = Array.make_matrix 4 4 zero in
  for i = 0 to 3 do
    for j = 0 to 3 do
      let s = ref zero in
      for k = 0 to 3 do
        s := add !s (mul a.(i).(k) b.(k).(j))
      done;
      r.(i).(j) <- !s
    done
  done;
  r

let () =
  try
    let e_cnt = Scanf.scanf " %d" (fun x -> x) in
    let j_mat = ref (Array.make_matrix 2 2 zero) in
    !j_mat.(0).(0) <- { re = 1.0; im = 0.0 };
    !j_mat.(1).(1) <- { re = 1.0; im = 0.0 };

    for _ = 1 to e_cnt do
      let kind = Scanf.scanf " %s" (fun s -> s) in
      let t_mat = Array.make_matrix 2 2 zero in
      if kind = "P" then begin
        let th = Scanf.scanf " %f" (fun x -> x) in
        let t = th *. pi /. 180.0 in
        let c = cos t in
        let s = sin t in
        t_mat.(0).(0) <- { re = c *. c; im = 0.0 };
        t_mat.(0).(1) <- { re = c *. s; im = 0.0 };
        t_mat.(1).(0) <- { re = c *. s; im = 0.0 };
        t_mat.(1).(1) <- { re = s *. s; im = 0.0 }
      end else if kind = "W" then begin
        let dl = Scanf.scanf " %f" (fun x -> x) in
        let th = Scanf.scanf " %f" (fun x -> x) in
        let d = dl *. pi /. 180.0 in
        let t = th *. pi /. 180.0 in
        let c = cos t in
        let s = sin t in
        let r_mat = Array.make_matrix 2 2 zero in
        r_mat.(0).(0) <- { re = c; im = 0.0 };
        r_mat.(0).(1) <- { re = s; im = 0.0 };
        r_mat.(1).(0) <- { re = -. s; im = 0.0 };
        r_mat.(1).(1) <- { re = c; im = 0.0 };
        let ri_mat = Array.make_matrix 2 2 zero in
        ri_mat.(0).(0) <- { re = c; im = 0.0 };
        ri_mat.(0).(1) <- { re = -. s; im = 0.0 };
        ri_mat.(1).(0) <- { re = s; im = 0.0 };
        ri_mat.(1).(1) <- { re = c; im = 0.0 };
        let d_mat = Array.make_matrix 2 2 zero in
        d_mat.(0).(0) <- polar 1.0 (d /. 2.0);
        d_mat.(1).(1) <- polar 1.0 (-. d /. 2.0);
        let prod = mmul ri_mat (mmul d_mat r_mat) in
        for i = 0 to 1 do
          for j = 0 to 1 do
            t_mat.(i).(j) <- prod.(i).(j)
          done
        done
      end else begin
        let ph = Scanf.scanf " %f" (fun x -> x) in
        let t = ph *. pi /. 180.0 in
        let c = cos t in
        let s = sin t in
        t_mat.(0).(0) <- { re = c; im = 0.0 };
        t_mat.(0).(1) <- { re = -. s; im = 0.0 };
        t_mat.(1).(0) <- { re = s; im = 0.0 };
        t_mat.(1).(1) <- { re = c; im = 0.0 }
      end;
      j_mat := mmul t_mat !j_mat
    done;

    let exr = Scanf.scanf " %f" (fun x -> x) in
    let exi = Scanf.scanf " %f" (fun x -> x) in
    let eyr = Scanf.scanf " %f" (fun x -> x) in
    let eyi = Scanf.scanf " %f" (fun x -> x) in

    let s_vec = Array.init 4 (fun _ -> Scanf.scanf " %f" (fun x -> x)) in

    let ein_x = { re = exr; im = exi } in
    let ein_y = { re = eyr; im = eyi } in
    let ox = add (mul !j_mat.(0).(0) ein_x) (mul !j_mat.(0).(1) ein_y) in
    let oy = add (mul !j_mat.(1).(0) ein_x) (mul !j_mat.(1).(1) ein_y) in

    let k_mat = Array.make_matrix 4 4 zero in
    for i = 0 to 1 do
      for j = 0 to 1 do
        for k = 0 to 1 do
          for l = 0 to 1 do
            k_mat.(i * 2 + k).(j * 2 + l) <- mul !j_mat.(i).(j) (conj !j_mat.(k).(l))
          done
        done
      done
    done;

    let a_mat = Array.make_matrix 4 4 zero in
    let ad = [|
      [| 1.0; 0.0; 0.0; 1.0 |];
      [| 1.0; 0.0; 0.0; -1.0 |];
      [| 0.0; 1.0; 1.0; 0.0 |];
      [| 0.0; 0.0; 0.0; 0.0 |]
    |] in
    for i = 0 to 3 do
      for j = 0 to 3 do
        a_mat.(i).(j) <- { re = ad.(i).(j); im = 0.0 }
      done
    done;
    a_mat.(3).(1) <- { re = 0.0; im = 1.0 };
    a_mat.(3).(2) <- { re = 0.0; im = -1.0 };

    let ai_mat = Array.make_matrix 4 4 zero in
    ai_mat.(0).(0) <- { re = 0.5; im = 0.0 };
    ai_mat.(0).(1) <- { re = 0.5; im = 0.0 };
    ai_mat.(1).(2) <- { re = 0.5; im = 0.0 };
    ai_mat.(1).(3) <- { re = 0.0; im = -0.5 };
    ai_mat.(2).(2) <- { re = 0.5; im = 0.0 };
    ai_mat.(2).(3) <- { re = 0.0; im = 0.5 };
    ai_mat.(3).(0) <- { re = 0.5; im = 0.0 };
    ai_mat.(3).(1) <- { re = -0.5; im = 0.0 };

    let m_mat = m4mul a_mat (m4mul k_mat ai_mat) in

    let o_vec = Array.make 4 0.0 in
    for i = 0 to 3 do
      for j = 0 to 3 do
        o_vec.(i) <- o_vec.(i) +. m_mat.(i).(j).re *. s_vec.(j)
      done
    done;

    let jv = [|
      !j_mat.(0).(0).re; !j_mat.(0).(0).im;
      !j_mat.(0).(1).re; !j_mat.(0).(1).im;
      !j_mat.(1).(0).re; !j_mat.(1).(0).im;
      !j_mat.(1).(1).re; !j_mat.(1).(1).im
    |] in
    print_string "JONES:";
    for i = 0 to 7 do
      Printf.printf " %.6f" (sn jv.(i))
    done;
    print_newline ();

    print_string "MUELLER:";
    for i = 0 to 3 do
      for j = 0 to 3 do
        Printf.printf " %.6f" (sn m_mat.(i).(j).re)
      done
    done;
    print_newline ();

    Printf.printf "OUT_J: %.6f %.6f %.6f %.6f\n" (sn ox.re) (sn ox.im) (sn oy.re) (sn oy.im);

    print_string "OUT_S:";
    for i = 0 to 3 do
      Printf.printf " %.6f" (sn o_vec.(i))
    done;
    print_newline ()
  with End_of_file -> ()
