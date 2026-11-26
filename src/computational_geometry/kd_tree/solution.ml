(* kd_tree: exact Berg 5.2 BUILDKDTREE + SEARCHKDTREE. Doubled coordinates
   make every region bound a closed integer bound. *)

type node =
  | Leaf of (int * int)
  | Split of { axis : int; split : int; left : node; right : node }

let rec kd_build pts depth =
  match pts with
  | [ p ] -> Leaf p
  | _ ->
      let ax = depth mod 2 in
      let srt =
        List.sort
          (fun (x1, y1) (x2, y2) ->
            if ax = 0 then compare x1 x2 else compare y1 y2)
          pts
      in
      let arr = Array.of_list srt in
      let mid = (Array.length arr - 1) / 2 in
      let sp = if ax = 0 then fst arr.(mid) else snd arr.(mid) in
      let l = Array.to_list (Array.sub arr 0 (mid + 1)) in
      let r =
        Array.to_list (Array.sub arr (mid + 1) (Array.length arr - mid - 1))
      in
      Split
        {
          axis = ax;
          split = sp;
          left = kd_build l (depth + 1);
          right = kd_build r (depth + 1);
        }

let rec kd_report node =
  match node with
  | Leaf p -> [ p ]
  | Split s -> kd_report s.left @ kd_report s.right

let kd_disjoint lo hi rl rh =
  match (lo, hi) with
  | _, Some h when h < rl -> true
  | Some l, _ when l > rh -> true
  | _ -> false

let kd_inside lo hi rl rh =
  match (lo, hi) with Some l, Some h -> l >= rl && h <= rh | _ -> false

let rec kd_visit node qxl qxr qyl qyh xlo xhi ylo yhi =
  if kd_disjoint xlo xhi qxl qxr || kd_disjoint ylo yhi qyl qyh then []
  else if kd_inside xlo xhi qxl qxr && kd_inside ylo yhi qyl qyh then
    kd_report node
  else
    match node with
    | Leaf (x, y) ->
        if qxl <= 2 * x && 2 * x <= qxr && qyl <= 2 * y && 2 * y <= qyh then
          [ (x, y) ]
        else []
    | Split s ->
        let s2 = 2 * s.split in
        if s.axis = 0 then
          kd_visit s.left qxl qxr qyl qyh xlo (Some s2) ylo yhi
          @ kd_visit s.right qxl qxr qyl qyh (Some (s2 + 1)) xhi ylo yhi
        else
          kd_visit s.left qxl qxr qyl qyh xlo xhi ylo (Some s2)
          @ kd_visit s.right qxl qxr qyl qyh xlo xhi (Some (s2 + 1)) yhi

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

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let nums = Array.of_list (List.map int_of_string toks) in
      let n = nums.(0) in
      let pts =
        List.init n (fun i -> (nums.(1 + (2 * i)), nums.(2 + (2 * i))))
      in
      let qpos = 1 + (2 * n) in
      let qn = if qpos < Array.length nums then nums.(qpos) else 0 in
      let root = if n > 0 then Some (kd_build pts 0) else None in
      let buf = Buffer.create 256 in
      for j = 0 to qn - 1 do
        let b = qpos + 1 + (4 * j) in
        let xl = nums.(b)
        and yl = nums.(b + 1)
        and xr = nums.(b + 2)
        and yh = nums.(b + 3) in
        let found =
          match root with
          | None -> []
          | Some r ->
              List.sort compare
                (kd_visit r (2 * xl) (2 * xr) (2 * yl) (2 * yh) None None None
                   None)
        in
        if j > 0 then Buffer.add_char buf '\n';
        Buffer.add_string buf (string_of_int (List.length found));
        match found with
        | [] -> ()
        | _ ->
            Buffer.add_char buf '\n';
            Buffer.add_string buf
              (String.concat " "
                 (List.map (fun (x, y) -> Printf.sprintf "%d,%d" x y) found))
      done;
      print_string (Buffer.contents buf)
