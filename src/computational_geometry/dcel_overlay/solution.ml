type pt = { x : int64; y : int64 }
type frac = { n : int64; d : int64 }

let cross a b c =
  let dx1 = Int64.sub b.x a.x in
  let dy1 = Int64.sub c.y a.y in
  let dy2 = Int64.sub b.y a.y in
  let dx2 = Int64.sub c.x a.x in
  Int64.sub (Int64.mul dx1 dy1) (Int64.mul dy2 dx2)

let proper a b c d =
  let d1 = cross c d a in
  let d2 = cross c d b in
  let d3 = cross a b c in
  let d4 = cross a b d in
  ((d1 > 0L && d2 < 0L) || (d1 < 0L && d2 > 0L)) &&
  ((d3 > 0L && d4 < 0L) || (d3 < 0L && d4 > 0L))

let tparam a b c d =
  let rx = Int64.sub b.x a.x in
  let ry = Int64.sub b.y a.y in
  let qx = Int64.sub d.x c.x in
  let qy = Int64.sub d.y c.y in
  let num = Int64.sub (Int64.mul (Int64.sub c.x a.x) qy) (Int64.mul (Int64.sub c.y a.y) qx) in
  let den = Int64.sub (Int64.mul rx qy) (Int64.mul ry qx) in
  if den < 0L then { n = Int64.neg num; d = Int64.neg den }
  else { n = num; d = den }

let flt a b =
  Int64.mul a.n b.d < Int64.mul b.n a.d

type xing = { e1 : int; e2 : int; t1 : frac; t2 : frac }
type ue = { u : int; v : int; poly : int; e : int }
type he = { from_v : int; to_v : int; twin : int; poly : int; e : int }

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
  if Array.length tokens >= 2 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let next_int64 () = Int64.of_string (next_str ()) in
    let n1 = next_int () in
    let p1 = Array.init n1 (fun _ ->
      let x = next_int64 () in
      let y = next_int64 () in
      { x; y }
    ) in
    let n2 = next_int () in
    let p2 = Array.init n2 (fun _ ->
      let x = next_int64 () in
      let y = next_int64 () in
      { x; y }
    ) in

    let xs = ref [] in
    for i = 0 to n1 - 1 do
      for j = 0 to n2 - 1 do
        let a = p1.(i) in
        let b = p1.((i + 1) mod n1) in
        let c = p2.(j) in
        let d = p2.((j + 1) mod n2) in
        if proper a b c d then
          xs := { e1 = i; e2 = j; t1 = tparam a b c d; t2 = tparam c d a b } :: !xs
      done
    done;
    let xs_arr = Array.of_list (List.rev !xs) in
    let x_count = Array.length xs_arr in
    let v_count = n1 + n2 + x_count in
    let xvid k = n1 + n2 + k in

    let on1 = Array.init n1 (fun _ -> ref []) in
    let on2 = Array.init n2 (fun _ -> ref []) in
    for k = 0 to x_count - 1 do
      on1.(xs_arr.(k).e1) := k :: !(on1.(xs_arr.(k).e1));
      on2.(xs_arr.(k).e2) := k :: !(on2.(xs_arr.(k).e2))
    done;

    for i = 0 to n1 - 1 do
      let sorted = List.sort (fun k l ->
        if flt xs_arr.(k).t1 xs_arr.(l).t1 then -1 else 1
      ) !(on1.(i)) in
      on1.(i) := sorted
    done;
    for j = 0 to n2 - 1 do
      let sorted = List.sort (fun k l ->
        if flt xs_arr.(k).t2 xs_arr.(l).t2 then -1 else 1
      ) !(on2.(j)) in
      on2.(j) := sorted
    done;

    let ues = ref [] in
    for i = 0 to n1 - 1 do
      let prev = ref i in
      List.iter (fun k ->
        ues := { u = !prev; v = xvid k; poly = 1; e = i } :: !ues;
        prev := xvid k
      ) !(on1.(i));
      ues := { u = !prev; v = (i + 1) mod n1; poly = 1; e = i } :: !ues
    done;
    for j = 0 to n2 - 1 do
      let prev = ref (n1 + j) in
      List.iter (fun k ->
        ues := { u = !prev; v = xvid k; poly = 2; e = j } :: !ues;
        prev := xvid k
      ) !(on2.(j));
      ues := { u = !prev; v = n1 + (j + 1) mod n2; poly = 2; e = j } :: !ues
    done;
    let ues_arr = Array.of_list (List.rev !ues) in
    let e_count = Array.length ues_arr in

    let hes = ref [] in
    for i = 0 to e_count - 1 do
      let ue = ues_arr.(i) in
      let k = List.length !hes in
      let h1 = { from_v = ue.u; to_v = ue.v; twin = k + 1; poly = ue.poly; e = ue.e } in
      let h2 = { from_v = ue.v; to_v = ue.u; twin = k; poly = ue.poly; e = ue.e } in
      hes := h2 :: h1 :: !hes
    done;
    let hes_arr = Array.of_list (List.rev !hes) in
    let h_count = Array.length hes_arr in

    let out = Array.init v_count (fun _ -> ref []) in
    for h = 0 to h_count - 1 do
      out.(hes_arr.(h).from_v) := h :: !(out.(hes_arr.(h).from_v))
    done;
    let out = Array.map (fun r -> Array.of_list (List.rev !r)) out in

    let ccw = Array.make v_count [||] in
    for v = 0 to n1 + n2 - 1 do
      let hs = Array.copy out.(v) in
      let dir_of h =
        let poly = hes_arr.(h).poly in
        let e = hes_arr.(h).e in
        if poly = 1 then
          let a = p1.(e) in
          let b = p1.((e + 1) mod n1) in
          let vv = p1.(v) in
          let w = if vv.x = a.x && vv.y = a.y then b else a in
          { x = Int64.sub w.x vv.x; y = Int64.sub w.y vv.y }
        else
          let vv = v - n1 in
          let a = p2.(e) in
          let b = p2.((e + 1) mod n2) in
          let cur = p2.(vv) in
          let w = if cur.x = a.x && cur.y = a.y then b else a in
          { x = Int64.sub w.x cur.x; y = Int64.sub w.y cur.y }
      in
      Array.sort (fun h1 h2 ->
        let d1 = dir_of h1 in
        let d2 = dir_of h2 in
        let q1 = if d1.y > 0L || (d1.y = 0L && d1.x > 0L) then 0 else 1 in
        let q2 = if d2.y > 0L || (d2.y = 0L && d2.x > 0L) then 0 else 1 in
        if q1 <> q2 then compare q1 q2
        else
          let cp = Int64.sub (Int64.mul d1.x d2.y) (Int64.mul d1.y d2.x) in
          if cp > 0L then -1
          else if cp < 0L then 1
          else 0
      ) hs;
      ccw.(v) <- hs
    done;

    for k = 0 to x_count - 1 do
      let v = xvid k in
      let e1 = xs_arr.(k).e1 in
      let e2 = xs_arr.(k).e2 in
      let a = p1.(e1) in
      let b = p1.((e1 + 1) mod n1) in
      let c = p2.(e2) in
      let ch1 = Array.of_list ([e1] @ (List.map xvid !(on1.(e1))) @ [(e1 + 1) mod n1]) in
      let ch2 = Array.of_list ([n1 + e2] @ (List.map xvid !(on2.(e2))) @ [n1 + (e2 + 1) mod n2]) in
      let i1 = ref 0 in
      while ch1.(!i1) <> v do incr i1 done;
      let i2 = ref 0 in
      while ch2.(!i2) <> v do incr i2 done;
      let toA = ch1.(!i1 - 1) in
      let toB = ch1.(!i1 + 1) in
      let toC = ch2.(!i2 - 1) in
      let toD = ch2.(!i2 + 1) in
      let hA = ref (-1) in
      let hB = ref (-1) in
      let hC = ref (-1) in
      let hD = ref (-1) in
      let out_v = out.(v) in
      for i = 0 to Array.length out_v - 1 do
        let h = out_v.(i) in
        let t = hes_arr.(h).to_v in
        if t = toA then hA := h
        else if t = toB then hB := h
        else if t = toC then hC := h
        else hD := h
      done;
      if cross a b c > 0L then
        ccw.(v) <- [| !hB; !hC; !hA; !hD |]
      else
        ccw.(v) <- [| !hB; !hD; !hA; !hC |]
    done;

    let pos = Array.make h_count (-1) in
    for v = 0 to v_count - 1 do
      let hs = ccw.(v) in
      for i = 0 to Array.length hs - 1 do
        pos.(hs.(i)) <- i
      done
    done;

    let nxt = Array.make h_count (-1) in
    for h = 0 to h_count - 1 do
      let w = hes_arr.(h).to_v in
      let t = hes_arr.(h).twin in
      let k = Array.length ccw.(w) in
      let p_idx = pos.(t) in
      nxt.(h) <- ccw.(w).((p_idx - 1 + k) mod k)
    done;

    let vis = Array.make h_count false in
    let cycles = ref 0 in
    for h = 0 to h_count - 1 do
      if not vis.(h) then begin
        incr cycles;
        let c = ref h in
        while not vis.(!c) do
          vis.(!c) <- true;
          c := nxt.(!c)
        done
      end
    done;

    let par = Array.init v_count (fun i -> i) in
    let rec find_root x =
      if par.(x) = x then x
      else begin
        par.(x) <- find_root par.(x);
        par.(x)
      end
    in
    for i = 0 to e_count - 1 do
      let ue = ues_arr.(i) in
      let ru = find_root ue.u in
      let rv = find_root ue.v in
      par.(ru) <- rv
    done;
    let comp_count = ref 0 in
    for v = 0 to v_count - 1 do
      if find_root v = v then incr comp_count
    done;

    let faces = !cycles - !comp_count + 1 in
    Printf.printf "%d %d %d\n" v_count e_count faces
  end
