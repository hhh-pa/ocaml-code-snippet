let sn x =
  let v = x +. 0.0 in
  if abs_float v < 5e-7 then 0.0 else v

let () =
  try
    let s_cnt = Scanf.scanf " %d" (fun x -> x) in
    let r = Array.make s_cnt 0.0 in
    let th = Array.make s_cnt 0.0 in
    let nd = Array.make s_cnt 0.0 in
    let dn = Array.make s_cnt 0.0 in
    for j = 0 to s_cnt - 1 do
      r.(j) <- Scanf.scanf " %f" (fun x -> x);
      th.(j) <- Scanf.scanf " %f" (fun x -> x);
      nd.(j) <- Scanf.scanf " %f" (fun x -> x);
      dn.(j) <- Scanf.scanf " %f" (fun x -> x)
    done;

    let h = ref (Scanf.scanf " %f" (fun x -> x)) in
    let u = ref (Scanf.scanf " %f" (fun x -> x)) in
    let hb = ref (Scanf.scanf " %f" (fun x -> x)) in
    let ub = ref (Scanf.scanf " %f" (fun x -> x)) in

    let big_h = !ub *. !h -. !u *. !hb in
    let n = ref 1.0 in
    let d = ref 0.0 in
    let s1 = ref 0.0 in
    let s2 = ref 0.0 in
    let s3 = ref 0.0 in
    let s4 = ref 0.0 in
    let s5 = ref 0.0 in
    let c1 = ref 0.0 in
    let c2 = ref 0.0 in

    for j = 0 to s_cnt - 1 do
      let c = if r.(j) = 0.0 then 0.0 else 1.0 /. r.(j) in
      let np = nd.(j) in
      let dp = dn.(j) in
      let up = (!n *. !u -. !h *. c *. (np -. !n)) /. np in
      let ubp = (!n *. !ub -. !hb *. c *. (np -. !n)) /. np in
      let a = !n *. (!h *. c +. !u) in
      let ab = !n *. (!hb *. c +. !ub) in
      let dun = up /. np -. !u /. !n in
      let dinv = 1.0 /. np -. 1.0 /. !n in
      let ddn = dp /. np -. !d /. !n in
      s1 := !s1 -. a *. a *. !h *. dun;
      s2 := !s2 -. a *. ab *. !h *. dun;
      s3 := !s3 -. ab *. ab *. !h *. dun;
      s4 := !s4 -. big_h *. big_h *. c *. dinv;
      if abs_float a > 1e-12 then
        s5 := !s5 -. (ab /. a) *. (ab *. ab *. !h *. dun +. big_h *. big_h *. c *. dinv);
      c1 := !c1 +. !h *. a *. ddn;
      c2 := !c2 +. !h *. ab *. ddn;
      h := !h +. up *. th.(j);
      u := up;
      hb := !hb +. ubp *. th.(j);
      ub := ubp;
      n := np;
      d := dp
    done;

    Printf.printf "SEIDEL: %.6f %.6f %.6f %.6f %.6f\n" (sn !s1) (sn !s2) (sn !s3) (sn !s4) (sn !s5);
    Printf.printf "CHROM: %.6f %.6f\n" (sn !c1) (sn !c2)
  with End_of_file -> ()
