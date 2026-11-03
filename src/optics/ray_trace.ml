let sn x =
  let v = x +. 0.0 in
  if abs_float v < 5e-7 then 0.0 else v

let () =
  try
    let s_cnt = Scanf.scanf " %d" (fun x -> x) in
    let zv = Array.make s_cnt 0.0 in
    let rr = Array.make s_cnt 0.0 in
    let na = Array.make s_cnt 0.0 in
    for j = 0 to s_cnt - 1 do
      zv.(j) <- Scanf.scanf " %f" (fun x -> x);
      rr.(j) <- Scanf.scanf " %f" (fun x -> x);
      na.(j) <- Scanf.scanf " %f" (fun x -> x)
    done;

    let m_cnt = Scanf.scanf " %d" (fun x -> x) in
    for m = 0 to m_cnt - 1 do
      let ox = ref (Scanf.scanf " %f" (fun x -> x)) in
      let oy = ref (Scanf.scanf " %f" (fun x -> x)) in
      let oz = ref (Scanf.scanf " %f" (fun x -> x)) in
      let dx = ref (Scanf.scanf " %f" (fun x -> x)) in
      let dy = ref (Scanf.scanf " %f" (fun x -> x)) in
      let dz = ref (Scanf.scanf " %f" (fun x -> x)) in

      let dl = sqrt (!dx *. !dx +. !dy *. !dy +. !dz *. !dz) in
      dx := !dx /. dl;
      dy := !dy /. dl;
      dz := !dz /. dl;

      let n1 = ref 1.0 in
      let opl = ref 0.0 in
      let ok = ref true in
      let hx = Array.make s_cnt 0.0 in
      let hy = Array.make s_cnt 0.0 in

      let j = ref 0 in
      while !j < s_cnt && !ok do
        let cur_j = !j in
        let t = ref (-1.0) in
        if rr.(cur_j) = 0.0 then begin
          if !dz <> 0.0 then
            t := (zv.(cur_j) -. !oz) /. !dz
        end else begin
          let cx = 0.0 in
          let cy = 0.0 in
          let cz = zv.(cur_j) +. rr.(cur_j) in
          let ocx = !ox -. cx in
          let ocy = !oy -. cy in
          let ocz = !oz -. cz in
          let b = !dx *. ocx +. !dy *. ocy +. !dz *. ocz in
          let c = ocx *. ocx +. ocy *. ocy +. ocz *. ocz -. rr.(cur_j) *. rr.(cur_j) in
          let disc = b *. b -. c in
          if disc >= 0.0 then begin
            let sq = sqrt disc in
            let t1 = -. b -. sq in
            let t2 = -. b +. sq in
            if t1 > 1e-9 then t := t1
            else if t2 > 1e-9 then t := t2
          end
        end;

        if !t < 1e-9 then begin
          ok := false
        end else begin
          opl := !opl +. !n1 *. !t;
          let px = !ox +. !dx *. !t in
          let py = !oy +. !dy *. !t in
          let pz = !oz +. !dz *. !t in
          hx.(cur_j) <- px;
          hy.(cur_j) <- py;

          let nx = ref 0.0 in
          let ny = ref 0.0 in
          let nz = ref 1.0 in
          if rr.(cur_j) <> 0.0 then begin
            let ar = abs_float rr.(cur_j) in
            nx := px /. ar;
            ny := py /. ar;
            nz := (pz -. (zv.(cur_j) +. rr.(cur_j))) /. ar;
            let nl = sqrt (!nx *. !nx +. !ny *. !ny +. !nz *. !nz) in
            nx := !nx /. nl;
            ny := !ny /. nl;
            nz := !nz /. nl
          end;

          if !dx *. !nx +. !dy *. !ny +. !dz *. !nz > 0.0 then begin
            nx := -. !nx;
            ny := -. !ny;
            nz := -. !nz
          end;

          let eta = !n1 /. na.(cur_j) in
          let c1 = -. (!dx *. !nx +. !dy *. !ny +. !dz *. !nz) in
          let s2t = eta *. eta *. (1.0 -. c1 *. c1) in
          if s2t > 1.0 then begin
            ok := false
          end else begin
            let c2 = sqrt (max 0.0 (1.0 -. s2t)) in
            let k = eta *. c1 -. c2 in
            dx := eta *. !dx +. k *. !nx;
            dy := eta *. !dy +. k *. !ny;
            dz := eta *. !dz +. k *. !nz;
            ox := px +. !dx *. 1e-9;
            oy := py +. !dy *. 1e-9;
            oz := pz +. !dz *. 1e-9;
            n1 := na.(cur_j)
          end
        end;
        incr j
      done;

      if not !ok then
        Printf.printf "RAY %d MISS\n" m
      else begin
        Printf.printf "RAY %d HIT:" m;
        for k = 0 to s_cnt - 1 do
          Printf.printf " %.6f %.6f" (sn hx.(k)) (sn hy.(k))
        done;
        print_newline ();
        Printf.printf "RAY %d DIR: %.6f %.6f %.6f\n" m (sn !dx) (sn !dy) (sn !dz);
        Printf.printf "RAY %d OPL: %.6f\n" m (sn !opl)
      end
    done
  with End_of_file -> ()
