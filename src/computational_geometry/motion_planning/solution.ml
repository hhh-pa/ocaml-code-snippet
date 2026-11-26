(* motion_planning: Berg 13.2 trapezoidal road map for rectangles + BFS. *)

let mp_vsealed xs ys rects rays k j =
  let x = xs.(k) in
  List.exists
    (fun (rx0, ry0, rx1, ry1) ->
      (rx0 = x || rx1 = x) && ry0 <= ys.(j) && ys.(j + 1) <= ry1)
    rects
  || List.exists
       (fun (ex, lo, hi) -> ex = x && lo <= ys.(j) && ys.(j + 1) <= hi)
       rays

let mp_hsealed xs ys rects i k =
  let y = ys.(k) in
  List.exists
    (fun (rx0, ry0, rx1, ry1) ->
      (ry0 = y || ry1 = y) && rx0 <= xs.(i) && xs.(i + 1) <= rx1)
    rects

let mp_open xs ys rects rays a b d =
  match d with
  | 0 -> not (mp_vsealed xs ys rects rays a b)
  | 1 -> not (mp_vsealed xs ys rects rays (a + 1) b)
  | 2 -> not (mp_hsealed xs ys rects a b)
  | _ -> not (mp_hsealed xs ys rects a (b + 1))

let mp_flood nx ny blocked xs ys rects rays face id i0 j0 =
  let di = [| -1; 1; 0; 0 |] and dj = [| 0; 0; -1; 1 |] in
  let stack = ref [ (i0, j0) ] in
  face.(i0).(j0) <- id;
  while !stack <> [] do
    let a, b = List.hd !stack in
    stack := List.tl !stack;
    for d = 0 to 3 do
      let na = a + di.(d) and nb = b + dj.(d) in
      if
        na >= 0 && nb >= 0 && na < nx && nb < ny
        && mp_open xs ys rects rays a b d
        && (not blocked.(na).(nb))
        && face.(na).(nb) < 0
      then begin
        face.(na).(nb) <- id;
        stack := (na, nb) :: !stack
      end
    done
  done

let mp_valid (bx0, by0, bx1, by1) rects px py =
  bx0 < px && px < bx1 && by0 < py && py < by1
  && not
       (List.exists
          (fun (rx0, ry0, rx1, ry1) ->
            rx0 <= px && px <= rx1 && ry0 <= py && py <= ry1)
          rects)

let mp_on_ray rays px py =
  List.exists (fun (ex, lo, hi) -> ex = px && lo < py && py < hi) rays

let mp_locate xs ys face px py =
  let found = ref (-1) in
  for i = 0 to Array.length xs - 2 do
    if xs.(i) <= px && px <= xs.(i + 1) then
      for j = 0 to Array.length ys - 2 do
        if ys.(j) <= py && py <= ys.(j + 1) && face.(i).(j) >= 0 then
          found := face.(i).(j)
      done
  done;
  !found

let mp_bfs adj fs fg =
  let n = Array.length adj in
  let prev = Array.make n (-1) in
  let queue = Queue.create () in
  Queue.add fs queue;
  prev.(fs) <- fs;
  while (not (Queue.is_empty queue)) && prev.(fg) < 0 do
    let v = Queue.take queue in
    List.iter
      (fun w ->
        if prev.(w) < 0 then begin
          prev.(w) <- v;
          Queue.add w queue
        end)
      adj.(v)
  done;
  if prev.(fg) < 0 then None
  else
    let path = ref [ fg ] in
    let cur = ref fg in
    while !cur <> fs do
      cur := prev.(!cur);
      path := !cur :: !path
    done;
    Some !path

let read_all () =
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_char buf (input_char stdin)
     done
   with End_of_file -> ());
  Buffer.contents buf

let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let split_ws s =
  let toks = ref [] in
  let cur = Buffer.create 16 in
  let flush () =
    if Buffer.length cur > 0 then begin
      toks := Buffer.contents cur :: !toks;
      Buffer.clear cur
    end
  in
  String.iter
    (fun c -> if is_space c then flush () else Buffer.add_char cur c)
    s;
  flush ();
  List.rev !toks

let list_uniq_sorted l =
  let rec aux acc = function
    | [] -> List.rev acc
    | [ x ] -> List.rev (x :: acc)
    | x :: (y :: _ as rest) ->
        if x = y then aux acc rest else aux (x :: acc) rest
  in
  aux [] (List.sort compare l)

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let nums = Array.of_list (List.map int_of_string toks) in
      let box = (nums.(0), nums.(1), nums.(2), nums.(3)) in
      let bx0, by0, bx1, by1 = box in
      let m = nums.(4) in
      let rects =
        List.init m (fun i ->
            ( nums.(5 + (4 * i)),
              nums.(6 + (4 * i)),
              nums.(7 + (4 * i)),
              nums.(8 + (4 * i)) ))
      in
      let qpos = 5 + (4 * m) in
      let qn = if qpos < Array.length nums then nums.(qpos) else 0 in
      let xs =
        Array.of_list
          (list_uniq_sorted
             ([ bx0; bx1 ]
             @ List.concat (List.map (fun (a, _, c, _) -> [ a; c ]) rects)))
      in
      let ys =
        Array.of_list
          (list_uniq_sorted
             ([ by0; by1 ]
             @ List.concat (List.map (fun (_, b, _, d) -> [ b; d ]) rects)))
      in
      let nx = Array.length xs - 1 and ny = Array.length ys - 1 in
      let blocked = Array.make_matrix nx ny false in
      for i = 0 to nx - 1 do
        for j = 0 to ny - 1 do
          if
            List.exists
              (fun (rx0, ry0, rx1, ry1) ->
                rx0 <= xs.(i)
                && xs.(i + 1) <= rx1
                && ry0 <= ys.(j)
                && ys.(j + 1) <= ry1)
              rects
          then blocked.(i).(j) <- true
        done
      done;
      let rays = ref [] in
      List.iter
        (fun (rx0, ry0, rx1, ry1) ->
          List.iter
            (fun exv ->
              if bx0 < exv && exv < bx1 then begin
                let hit_up = ref None and hit_dn = ref None in
                List.iter
                  (fun (qx0, qy0, qx1, qy1) ->
                    if qx0 < exv && exv < qx1 then
                      List.iter
                        (fun y ->
                          (if y > ry1 then
                             match !hit_up with
                             | None -> hit_up := Some y
                             | Some h -> if y < h then hit_up := Some y);
                          if y < ry0 then
                            match !hit_dn with
                            | None -> hit_dn := Some y
                            | Some h -> if y > h then hit_dn := Some y)
                        [ qy0; qy1 ])
                  rects;
                (if by1 > ry1 then
                   match !hit_up with
                   | None -> hit_up := Some by1
                   | Some h -> if by1 < h then hit_up := Some by1);
                (if by0 < ry0 then
                   match !hit_dn with
                   | None -> hit_dn := Some by0
                   | Some h -> if by0 > h then hit_dn := Some by0);
                (match !hit_up with
                | Some h -> rays := (exv, ry1, h) :: !rays
                | None -> ());
                match !hit_dn with
                | Some h -> rays := (exv, h, ry0) :: !rays
                | None -> ()
              end)
            [ rx0; rx1 ])
        rects;
      let rays = List.rev !rays in
      let face = Array.make_matrix nx ny (-1) in
      let nfaces = ref 0 in
      for i = 0 to nx - 1 do
        for j = 0 to ny - 1 do
          if (not blocked.(i).(j)) && face.(i).(j) < 0 then begin
            mp_flood nx ny blocked xs ys rects rays face !nfaces i j;
            incr nfaces
          end
        done
      done;
      let nfaces = !nfaces in
      let fx0 = Array.make nfaces nx
      and fx1 = Array.make nfaces (-1)
      and fy0 = Array.make nfaces ny
      and fy1 = Array.make nfaces (-1) in
      for i = 0 to nx - 1 do
        for j = 0 to ny - 1 do
          let f = face.(i).(j) in
          if f >= 0 then begin
            fx0.(f) <- min fx0.(f) i;
            fx1.(f) <- max fx1.(f) i;
            fy0.(f) <- min fy0.(f) j;
            fy1.(f) <- max fy1.(f) j
          end
        done
      done;
      let coords =
        Array.init
          (nfaces + List.length rays)
          (fun v ->
            if v < nfaces then
              (xs.(fx0.(v)) + xs.(fx1.(v) + 1), ys.(fy0.(v)) + ys.(fy1.(v) + 1))
            else
              let ex, lo, hi = List.nth rays (v - nfaces) in
              (2 * ex, lo + hi))
      in
      let adj = Array.make (nfaces + List.length rays) [] in
      let xindex = Hashtbl.create 16 in
      Array.iteri (fun k x -> Hashtbl.add xindex x k) xs;
      List.iteri
        (fun e (ex, lo, hi) ->
          let k = Hashtbl.find xindex ex in
          let sides = ref [] in
          for j = 0 to ny - 1 do
            if lo <= ys.(j) && ys.(j + 1) <= hi then
              List.iter
                (fun f ->
                  if f >= 0 && not (List.mem f !sides) then sides := f :: !sides)
                [ face.(k - 1).(j); face.(k).(j) ]
          done;
          let node = nfaces + e in
          List.iter
            (fun f ->
              adj.(f) <- node :: adj.(f);
              adj.(node) <- f :: adj.(node))
            !sides)
        rays;
      Array.iteri
        (fun v lst ->
          adj.(v) <- List.sort (fun a b -> compare coords.(a) coords.(b)) lst)
        adj;
      let buf = Buffer.create 256 in
      let first = ref true in
      for qi = 0 to qn - 1 do
        let b = qpos + 1 + (4 * qi) in
        let sx = nums.(b)
        and sy = nums.(b + 1)
        and gx = nums.(b + 2)
        and gy = nums.(b + 3) in
        if !first then first := false else Buffer.add_char buf '\n';
        if
          (not (mp_valid box rects sx sy))
          || (not (mp_valid box rects gx gy))
          || mp_on_ray rays sx sy || mp_on_ray rays gx gy
        then Buffer.add_string buf "FORBIDDEN"
        else
          let fs = mp_locate xs ys face sx sy
          and fg = mp_locate xs ys face gx gy in
          if fs < 0 || fg < 0 then Buffer.add_string buf "FORBIDDEN"
          else if fs = fg then begin
            Buffer.add_string buf "2";
            Buffer.add_char buf '\n';
            Printf.bprintf buf "%d %d\n%d %d" (2 * sx) (2 * sy) (2 * gx) (2 * gy)
          end
          else
            match mp_bfs adj fs fg with
            | None -> Buffer.add_string buf "NO PATH"
            | Some path ->
                let pts =
                  ((2 * sx, 2 * sy) :: List.map (fun v -> coords.(v)) path)
                  @ [ (2 * gx, 2 * gy) ]
                in
                Buffer.add_string buf (string_of_int (List.length pts));
                List.iter (fun (x, y) -> Printf.bprintf buf "\n%d %d" x y) pts
      done;
      print_string (Buffer.contents buf)
