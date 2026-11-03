let holds a b op =
  if op = 0 then a <> b
  else if op = 1 then a = b
  else if op = 2 then a < b
  else a > b

let solve_case () =
  let n = Scanf.scanf " %d" (fun x -> x) in
  let par = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
  let dom = Array.make n [] in
  for i = 0 to n - 1 do
    let s = Scanf.scanf " %d" (fun x -> x) in
    let d = ref [] in
    for _ = 0 to s - 1 do
      d := Scanf.scanf " %d" (fun x -> x) :: !d
    done;
    dom.(i) <- List.sort compare !d
  done;
  let op = Array.make n (-1) in
  for i = 0 to n - 1 do
    if par.(i) >= 0 then
      op.(i) <- Scanf.scanf " %d" (fun x -> x)
  done;
  let kids = Array.make n [] in
  let root = ref (-1) in
  for i = 0 to n - 1 do
    if par.(i) < 0 then root := i
    else kids.(par.(i)) <- i :: kids.(par.(i))
  done;
  let order = ref [] in
  let st = ref [!root] in
  while !st <> [] do
    let v = List.hd !st in
    st := List.tl !st;
    order := v :: !order;
    List.iter (fun k -> st := k :: !st) kids.(v)
  done;
  let order_arr = Array.of_list (List.rev !order) in

  let ok = ref true in
  let oi = ref (Array.length order_arr - 1) in
  while !oi >= 0 && !ok do
    let v = order_arr.(!oi) in
    if par.(v) >= 0 then begin
      let p = par.(v) in
      let nd = List.filter (fun x ->
        List.exists (fun y -> holds x y op.(v)) dom.(v)
      ) dom.(p) in
      dom.(p) <- nd;
      if nd = [] then ok := false
    end;
    decr oi
  done;

  let asn = Array.make n (-1) in
  if !ok then begin
    let i = ref 0 in
    while !i < Array.length order_arr && !ok do
      let v = order_arr.(!i) in
      if par.(v) < 0 then begin
        if dom.(v) = [] then ok := false
        else asn.(v) <- List.hd dom.(v)
      end else begin
        let p_val = asn.(par.(v)) in
        let op_v = op.(v) in
        match List.find_opt (fun y -> holds p_val y op_v) dom.(v) with
        | Some y -> asn.(v) <- y
        | None -> ok := false
      end;
      incr i
    done
  end;

  if not !ok then
    Printf.printf "TREECSP INCONSISTENT\n"
  else begin
    Printf.printf "TREECSP";
    for i = 0 to n - 1 do
      Printf.printf " %d" asn.(i)
    done;
    print_newline ()
  end

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
