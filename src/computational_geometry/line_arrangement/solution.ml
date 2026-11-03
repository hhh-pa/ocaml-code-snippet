let llgcd a_in b_in =
  let a = ref (if a_in < 0L then Int64.neg a_in else a_in) in
  let b = ref (if b_in < 0L then Int64.neg b_in else b_in) in
  while !b <> 0L do
    let t = Int64.rem !a !b in
    a := !b;
    b := t
  done;
  !a

type vertex = {
  xn : int64;
  xd : int64;
  yn : int64;
  yd : int64;
}

let cmp_v a b =
  let l1 = Int64.mul a.xn b.xd in
  let r1 = Int64.mul b.xn a.xd in
  if l1 <> r1 then Int64.compare l1 r1
  else
    let l2 = Int64.mul a.yn b.yd in
    let r2 = Int64.mul b.yn a.yd in
    Int64.compare l2 r2

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
    let big_a = Array.make n 0L in
    let big_b = Array.make n 0L in
    let big_c = Array.make n 0L in
    for i = 0 to n - 1 do
      big_a.(i) <- next_int64 ();
      big_b.(i) <- next_int64 ();
      big_c.(i) <- next_int64 ()
    done;

    let verts = ref [] in
    for i = 0 to n - 1 do
      for j = i + 1 to n - 1 do
        let det = Int64.sub (Int64.mul big_a.(i) big_b.(j)) (Int64.mul big_a.(j) big_b.(i)) in
        if det <> 0L then begin
          let xn = Int64.sub (Int64.mul big_c.(i) big_b.(j)) (Int64.mul big_c.(j) big_b.(i)) in
          let yn = Int64.sub (Int64.mul big_a.(i) big_c.(j)) (Int64.mul big_a.(j) big_c.(i)) in
          let gx = llgcd xn det in
          let gy = llgcd yn det in
          let xd = if det < 0L then Int64.neg det else det in
          let xn_signed = if det < 0L then Int64.neg xn else xn in
          let yn_signed = if det < 0L then Int64.neg yn else yn in
          let xn2 = Int64.div xn_signed gx in
          let xd2 = Int64.div xd gx in
          let yn2 = Int64.div yn_signed gy in
          let yd2 = Int64.div xd gy in
          verts := { xn = xn2; xd = xd2; yn = yn2; yd = yd2 } :: !verts
        end
      done
    done;

    let verts_arr = Array.of_list !verts in
    Array.sort cmp_v verts_arr;
    let vv = Array.length verts_arr in
    Printf.printf "%d\n" vv;
    for i = 0 to vv - 1 do
      let v = verts_arr.(i) in
      Printf.printf "%Ld/%Ld %Ld/%Ld\n" v.xn v.xd v.yn v.yd
    done;
    let edges = n + 2 * vv in
    let faces = n + vv + 1 in
    Printf.printf "%d %d\n" edges faces
  end
