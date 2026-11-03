type point = { x : int64; y : int64 }
type disc = { cx : float; cy : float; r2 : float }

let cmp_pt a b =
  let c = Int64.compare a.x b.x in
  if c <> 0 then c else Int64.compare a.y b.y

let dist_sq x1 y1 x2 y2 =
  let dx = x1 -. x2 in
  let dy = y1 -. y2 in
  dx *. dx +. dy *. dy

let in_disc d (p : point) =
  dist_sq d.cx d.cy (Int64.to_float p.x) (Int64.to_float p.y) <= d.r2 +. 1e-9

let disc_from_2 (a : point) (b : point) =
  let ax = Int64.to_float a.x in
  let ay = Int64.to_float a.y in
  let bx = Int64.to_float b.x in
  let by = Int64.to_float b.y in
  let cx = 0.5 *. (ax +. bx) in
  let cy = 0.5 *. (ay +. by) in
  let dx = bx -. ax in
  let dy = by -. ay in
  { cx; cy; r2 = 0.25 *. (dx *. dx +. dy *. dy) }

let disc_from_3 (a : point) (b : point) (c : point) =
  let bx_i = Int64.sub b.x a.x in
  let by_i = Int64.sub b.y a.y in
  let cx_i = Int64.sub c.x a.x in
  let cy_i = Int64.sub c.y a.y in
  let det = Int64.sub (Int64.mul bx_i cy_i) (Int64.mul by_i cx_i) in
  if det = 0L then begin
    let d1 = disc_from_2 a b in
    let d2 = disc_from_2 a c in
    let d3 = disc_from_2 b c in
    let best = ref d1 in
    if d2.r2 > !best.r2 then best := d2;
    if d3.r2 > !best.r2 then best := d3;
    !best
  end else begin
    let bx = Int64.to_float bx_i in
    let by = Int64.to_float by_i in
    let cx = Int64.to_float cx_i in
    let cy = Int64.to_float cy_i in
    let b2 = bx *. bx +. by *. by in
    let c2 = cx *. cx +. cy *. cy in
    let d = 2.0 *. Int64.to_float det in
    let ux = (cy *. b2 -. by *. c2) /. d in
    let uy = (bx *. c2 -. cx *. b2) /. d in
    {
      cx = Int64.to_float a.x +. ux;
      cy = Int64.to_float a.y +. uy;
      r2 = ux *. ux +. uy *. uy;
    }
  end

let mini_disc_with_2_points pts limit q1 q2 =
  let d = ref (disc_from_2 q1 q2) in
  for k = 0 to limit - 1 do
    if not (in_disc !d pts.(k)) then
      d := disc_from_3 q1 q2 pts.(k)
  done;
  !d

let mini_disc_with_point pts limit q =
  let d = ref (disc_from_2 pts.(0) q) in
  for j = 1 to limit - 1 do
    if not (in_disc !d pts.(j)) then
      d := mini_disc_with_2_points pts j pts.(j) q
  done;
  !d

let welzl_mini_disc pts =
  let n = Array.length pts in
  if n = 0 then { cx = 0.0; cy = 0.0; r2 = 0.0 }
  else if n = 1 then { cx = Int64.to_float pts.(0).x; cy = Int64.to_float pts.(0).y; r2 = 0.0 }
  else begin
    let d = ref (disc_from_2 pts.(0) pts.(1)) in
    for i = 2 to n - 1 do
      if not (in_disc !d pts.(i)) then
        d := mini_disc_with_point pts i pts.(i)
    done;
    !d
  end

let llround val_f =
  if val_f >= 0.0 then Int64.of_float (floor (val_f +. 0.5))
  else Int64.of_float (ceil (val_f -. 0.5))

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
      let pts = Array.init n (fun _ ->
        let x = next_int64 () in
        let y = next_int64 () in
        { x; y }
      ) in
      Array.sort cmp_pt pts;
      let uniq = ref [] in
      for i = 0 to n - 1 do
        match !uniq with
        | prev :: _ when prev.x = pts.(i).x && prev.y = pts.(i).y -> ()
        | _ -> uniq := pts.(i) :: !uniq
      done;
      let pts = Array.of_list (List.rev !uniq) in
      let ans = welzl_mini_disc pts in
      let cx_out = llround (ans.cx *. 1000.0) in
      let cy_out = llround (ans.cy *. 1000.0) in
      let r2_out = llround (ans.r2 *. 1000.0) in
      Printf.printf "%Ld %Ld %Ld\n" cx_out cy_out r2_out
    end
  end
