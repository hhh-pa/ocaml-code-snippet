(* Trapezoidal map + point-location DAG *)

type pt = { x : int64; y : int64 }
type seg = { a : pt; b : pt }

let cross a b c =
  let x1 = Int64.sub b.x a.x in
  let y1 = Int64.sub b.y a.y in
  let x2 = Int64.sub c.x a.x in
  let y2 = Int64.sub c.y a.y in
  Int64.sub (Int64.mul x1 y2) (Int64.mul y1 x2)

type trap = {
  mutable top : int;
  mutable bot : int;
  mutable lp : pt;
  mutable rp : pt;
}

type node = {
  mutable kind : int;
  mutable trap : int;
  mutable p : pt;
  mutable seg : int;
  mutable left : int;
  mutable right : int;
}

type 'a dyn_array = {
  mutable data : 'a array;
  mutable len : int;
}

let make_dyn cap dummy = { data = Array.make cap dummy; len = 0 }

let push da x =
  if da.len = Array.length da.data then begin
    let ncap = max 4 (da.len * 2) in
    let nd = Array.make ncap da.data.(0) in
    Array.blit da.data 0 nd 0 da.len;
    da.data <- nd
  end;
  let idx = da.len in
  da.data.(idx) <- x;
  da.len <- da.len + 1;
  idx

let () =
  let buf = Buffer.create 4096 in
  let b = Bytes.create 4096 in
  let rec read_all () =
    let n = input stdin b 0 4096 in
    if n > 0 then begin
      Buffer.add_subbytes buf b 0 n;
      read_all ()
    end
  in
  (try read_all () with End_of_file -> ());
  let content = Buffer.contents buf in
  let tokens =
    let len = String.length content in
    let rec collect acc i =
      if i >= len then List.rev acc
      else if content.[i] <= ' ' then collect acc (i + 1)
      else
        let j = ref i in
        while !j < len && content.[!j] > ' ' do incr j done;
        collect (String.sub content i (!j - i) :: acc) !j
    in
    Array.of_list (collect [] 0)
  in
  let pos = ref 0 in
  let next_token () =
    if !pos < Array.length tokens then
      let t = tokens.(!pos) in
      incr pos;
      t
    else raise End_of_file
  in
  try
    let s_str = next_token () in
    let s = int_of_string s_str in
    let segs = Array.init s (fun _ ->
      let x1 = Int64.of_string (next_token ()) in
      let y1 = Int64.of_string (next_token ()) in
      let x2 = Int64.of_string (next_token ()) in
      let y2 = Int64.of_string (next_token ()) in
      if x1 > x2 then
        { a = { x = x2; y = y2 }; b = { x = x1; y = y1 } }
      else
        { a = { x = x1; y = y1 }; b = { x = x2; y = y2 } }
    ) in
    let ord = Array.init s (fun i -> i) in
    let st = ref 192L in
    let mult = 6364136223846793005L in
    let add = 1442695040888963407L in
    let rnd () =
      st := Int64.add (Int64.mul !st mult) add;
      Int64.shift_right_logical !st 33
    in
    for i = s - 1 downto 1 do
      let r = rnd () in
      let j = Int64.to_int (Int64.rem r (Int64.of_int (i + 1))) in
      let tmp = ord.(i) in
      ord.(i) <- ord.(j);
      ord.(j) <- tmp
    done;

    let dummy_pt = { x = 0L; y = 0L } in
    let dummy_trap = { top = -1; bot = -1; lp = dummy_pt; rp = dummy_pt } in
    let dummy_node = { kind = -1; trap = -1; p = dummy_pt; seg = -1; left = -1; right = -1 } in
    let traps = make_dyn 16 dummy_trap in
    let nodes = make_dyn 16 dummy_node in

    let x0 = ref 4000000000000L in
    let x1 = ref (-4000000000000L) in
    for i = 0 to s - 1 do
      let min_x = min segs.(i).a.x segs.(i).b.x in
      let max_x = max segs.(i).a.x segs.(i).b.x in
      x0 := min !x0 (Int64.sub min_x 1L);
      x1 := max !x1 (Int64.add max_x 1L)
    done;

    ignore (push traps { top = -1; bot = -1; lp = { x = !x0; y = 0L }; rp = { x = !x1; y = 0L } });
    ignore (push nodes { kind = 0; trap = 0; p = dummy_pt; seg = -1; left = -1; right = -1 });

    let above si p =
      let a = segs.(si).a in
      let b = segs.(si).b in
      if a.x = b.x then false
      else
        let c = cross a b p in
        c > 0L
    in

    for oi = 0 to s - 1 do
      let si = ord.(oi) in
      let p = segs.(si).a in
      let q = segs.(si).b in

      let leaves = ref [] in
      for ni = 0 to nodes.len - 1 do
        if nodes.data.(ni).kind = 0 then
          leaves := ni :: !leaves
      done;
      let leaves = List.rev !leaves in

      let hits = ref [] in
      List.iter (fun ni ->
        let t_trap = traps.data.(nodes.data.(ni).trap) in
        if t_trap.rp.x <= p.x || t_trap.lp.x >= q.x then ()
        else begin
          let xm = (max (Int64.to_float t_trap.lp.x) (Int64.to_float p.x) +.
                    min (Int64.to_float t_trap.rp.x) (Int64.to_float q.x)) /. 2.0 in
          let t = (xm -. Int64.to_float p.x) /. (Int64.to_float q.x -. Int64.to_float p.x) in
          let ys = Int64.to_float p.y +. t *. (Int64.to_float q.y -. Int64.to_float p.y) in
          let yt = ref 4e18 in
          let yb = ref (-4e18) in
          if t_trap.top >= 0 then begin
            let sg = segs.(t_trap.top) in
            let tt = (xm -. Int64.to_float sg.a.x) /. (Int64.to_float sg.b.x -. Int64.to_float sg.a.x) in
            yt := Int64.to_float sg.a.y +. tt *. (Int64.to_float sg.b.y -. Int64.to_float sg.a.y)
          end;
          if t_trap.bot >= 0 then begin
            let sg = segs.(t_trap.bot) in
            let tt = (xm -. Int64.to_float sg.a.x) /. (Int64.to_float sg.b.x -. Int64.to_float sg.a.x) in
            yb := Int64.to_float sg.a.y +. tt *. (Int64.to_float sg.b.y -. Int64.to_float sg.a.y)
          end;
          if !yb < ys && ys < !yt then
            hits := ni :: !hits
        end
      ) leaves;

      let hits_arr = Array.of_list !hits in
      Array.sort (fun a b ->
        Int64.compare traps.data.(nodes.data.(a).trap).lp.x traps.data.(nodes.data.(b).trap).lp.x
      ) hits_arr;

      let nh = Array.length hits_arr in
      for hi = 0 to nh - 1 do
        let ni = hits_arr.(hi) in
        let t_trap = traps.data.(nodes.data.(ni).trap) in
        let first = (hi = 0) in
        let last = (hi = nh - 1) in
        let top = t_trap.top in
        let bot = t_trap.bot in

        let ulp = if t_trap.lp.x < p.x then p else t_trap.lp in
        let urp = if t_trap.rp.x > q.x then q else t_trap.rp in

        let up_t = push traps { top = top; bot = si; lp = ulp; rp = urp } in
        let lo_t = push traps { top = si; bot = bot; lp = ulp; rp = urp } in

        let up_n = push nodes { kind = 0; trap = up_t; p = dummy_pt; seg = -1; left = -1; right = -1 } in
        let lo_n = push nodes { kind = 0; trap = lo_t; p = dummy_pt; seg = -1; left = -1; right = -1 } in

        let y_n = push nodes { kind = 2; trap = -1; p = dummy_pt; seg = si; left = up_n; right = lo_n } in
        let rep = ref y_n in

        if first && t_trap.lp.x < p.x then begin
          let l_t = push traps { top = top; bot = bot; lp = t_trap.lp; rp = p } in
          let l_n = push nodes { kind = 0; trap = l_t; p = dummy_pt; seg = -1; left = -1; right = -1 } in
          let x_n = push nodes { kind = 1; trap = -1; p = p; seg = -1; left = l_n; right = !rep } in
          rep := x_n
        end;

        if last && t_trap.rp.x > q.x then begin
          let r_t = push traps { top = top; bot = bot; lp = q; rp = t_trap.rp } in
          let r_n = push nodes { kind = 0; trap = r_t; p = dummy_pt; seg = -1; left = -1; right = -1 } in
          let x_n = push nodes { kind = 1; trap = -1; p = q; seg = -1; left = !rep; right = r_n } in
          rep := x_n
        end;

        nodes.data.(ni).kind <- 3;
        nodes.data.(ni).left <- !rep
      done
    done;

    let rec query ni (qq : pt) =
      let nd = nodes.data.(ni) in
      if nd.kind = 0 then nd.trap
      else if nd.kind = 3 then query nd.left qq
      else if nd.kind = 1 then
        if qq.x < nd.p.x then query nd.left qq else query nd.right qq
      else
        if above nd.seg qq then query nd.left qq else query nd.right qq
    in

    let qq = int_of_string (next_token ()) in
    for _ = 0 to qq - 1 do
      let gx = Int64.of_string (next_token ()) in
      let gy = Int64.of_string (next_token ()) in
      let trap_idx = query 0 { x = gx; y = gy } in
      let t = traps.data.(trap_idx) in
      Printf.printf "%d %d\n" t.top t.bot
    done
  with End_of_file -> ()
