let gcd_ll a_in b_in =
  let a = ref (if a_in < 0L then Int64.neg a_in else a_in) in
  let b = ref (if b_in < 0L then Int64.neg b_in else b_in) in
  while !b <> 0L do
    let t = Int64.rem !a !b in
    a := !b;
    b := t
  done;
  !a

type half_plane = {
  dx : int64;
  dy : int64;
  c : int64;
}

type point_d = {
  x : float;
  y : float;
}

let half h =
  if h.dy > 0L || (h.dy = 0L && h.dx > 0L) then 0 else 1

let cmp_hp h1 h2 =
  let half1 = half h1 in
  let half2 = half h2 in
  if half1 <> half2 then compare half1 half2
  else
    let cr = Int64.sub (Int64.mul h1.dx h2.dy) (Int64.mul h1.dy h2.dx) in
    if cr <> 0L then (if cr > 0L then -1 else 1)
    else Int64.compare h2.c h1.c

let is_outside h p =
  let lhs = -. (Int64.to_float h.dy) *. p.x +. (Int64.to_float h.dx) *. p.y in
  lhs < (Int64.to_float h.c) -. 1e-9

let intersect h1 h2 =
  let dx1 = Int64.to_float h1.dx in
  let dy1 = Int64.to_float h1.dy in
  let dx2 = Int64.to_float h2.dx in
  let dy2 = Int64.to_float h2.dy in
  let c1 = Int64.to_float h1.c in
  let c2 = Int64.to_float h2.c in
  let det = dx1 *. dy2 -. dy1 *. dx2 in
  let x = (c1 *. dx2 -. dx1 *. c2) /. det in
  let y = (c1 *. dy2 -. dy1 *. c2) /. det in
  { x; y }

let llround x =
  if x >= 0.0 then Int64.of_float (floor (x +. 0.5))
  else Int64.of_float (ceil (x -. 0.5))

let () =
  let tokens =
    let buf = Buffer.create 4096 in
    (try while true do Buffer.add_channel buf stdin 4096 done with End_of_file -> ());
    let s = Buffer.contents buf in
    let len = String.length s in
    let res = ref [] in
    let i = ref 0 in
    while !i < len do
      while !i < len && (let c = s.[!i] in c = ' ' || c = '\t' || c = '\r' || c = '\n') do incr i done;
      if !i < len then begin
        let st = !i in
        while !i < len && (let c = s.[!i] in c <> ' ' && c <> '\t' && c <> '\r' && c <> '\n') do incr i done;
        res := String.sub s st (!i - st) :: !res
      end
    done;
    Array.of_list (List.rev !res)
  in
  if Array.length tokens >= 1 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let next_int64 () = Int64.of_string (next_str ()) in
    let n = next_int () in
    if n > 0 then begin
      let raw = ref [] in
      for _ = 0 to n - 1 do
        let x1 = next_int64 () in
        let y1 = next_int64 () in
        let x2 = next_int64 () in
        let y2 = next_int64 () in
        let dx = Int64.sub x2 x1 in
        let dy = Int64.sub y2 y1 in
        let g = gcd_ll dx dy in
        if g <> 0L then begin
          let dx = Int64.div dx g in
          let dy = Int64.div dy g in
          let c = Int64.sub (Int64.mul dx y1) (Int64.mul dy x1) in
          raw := { dx; dy; c } :: !raw
        end
      done;
      let raw_arr = Array.of_list !raw in
      Array.sort cmp_hp raw_arr;

      let hps = ref [] in
      for i = 0 to Array.length raw_arr - 1 do
        let h = raw_arr.(i) in
        match !hps with
        | prev :: _ when prev.dx = h.dx && prev.dy = h.dy -> ()
        | _ -> hps := h :: !hps
      done;
      let hps = Array.of_list (List.rev !hps) in

      let cap = 2 * (Array.length hps) + 16 in
      let dq = Array.make cap { dx = 0L; dy = 0L; c = 0L } in
      let head = ref 0 in
      let tail = ref 0 in

      let dq_size () = !tail - !head in
      let dq_back () = dq.(!tail - 1) in
      let dq_back2 () = dq.(!tail - 2) in
      let dq_front () = dq.(!head) in
      let dq_front2 () = dq.(!head + 1) in
      let dq_pop_back () = decr tail in
      let dq_pop_front () = incr head in
      let dq_push_back h = dq.(!tail) <- h; incr tail in

      let failed = ref false in
      for i = 0 to Array.length hps - 1 do
        let h = hps.(i) in
        if not !failed then begin
          while dq_size () >= 2 && is_outside h (intersect (dq_back2 ()) (dq_back ())) do
            dq_pop_back ()
          done;
          while dq_size () >= 2 && is_outside h (intersect (dq_front ()) (dq_front2 ())) do
            dq_pop_front ()
          done;
          if dq_size () > 0 then begin
            let b = dq_back () in
            let cr = Int64.sub (Int64.mul b.dx h.dy) (Int64.mul b.dy h.dx) in
            if cr = 0L then failed := true
          end;
          dq_push_back h
        end
      done;

      if not !failed then begin
        while dq_size () >= 3 && is_outside (dq_front ()) (intersect (dq_back2 ()) (dq_back ())) do
          dq_pop_back ()
        done;
        while dq_size () >= 3 && is_outside (dq_back ()) (intersect (dq_front ()) (dq_front2 ())) do
          dq_pop_front ()
        done
      end;

      if !failed || dq_size () < 3 then
        print_endline "0 0"
      else begin
        let m = dq_size () in
        let verts = ref [] in
        let valid = ref true in
        for i = 0 to m - 1 do
          let h1 = dq.(!head + i) in
          let h2 = dq.(!head + ((i + 1) mod m)) in
          let cr = Int64.sub (Int64.mul h1.dx h2.dy) (Int64.mul h1.dy h2.dx) in
          if cr <= 0L then valid := false
          else begin
            let pt = intersect h1 h2 in
            match !verts with
            | [] -> verts := pt :: !verts
            | prev :: _ ->
              if hypot (pt.x -. prev.x) (pt.y -. prev.y) > 1e-7 then
                verts := pt :: !verts
          end
        done;
        let verts_arr = Array.of_list (List.rev !verts) in
        let vlen = ref (Array.length verts_arr) in
        if !vlen >= 2 && hypot (verts_arr.(0).x -. verts_arr.(!vlen - 1).x) (verts_arr.(0).y -. verts_arr.(!vlen - 1).y) <= 1e-7 then
          decr vlen;
        if not !valid || !vlen < 3 then
          print_endline "0 0"
        else begin
          let area2 = ref 0.0 in
          for i = 0 to !vlen - 1 do
            let a = verts_arr.(i) in
            let b = verts_arr.((i + 1) mod !vlen) in
            area2 := !area2 +. (a.x *. b.y -. a.y *. b.x)
          done;
          let area = 0.5 *. abs_float !area2 in
          if area <= 1e-9 then print_endline "0 0"
          else begin
            let area_out = llround (area *. 1000.0) in
            Printf.printf "%d %Ld\n" !vlen area_out
          end
        end
      end
    end
  end
