type point = { x : int64; y : int64 }

let cross a b c =
  let dx1 = Int64.sub b.x a.x in
  let dy1 = Int64.sub c.y a.y in
  let dy2 = Int64.sub b.y a.y in
  let dx2 = Int64.sub c.x a.x in
  Int64.sub (Int64.mul dx1 dy1) (Int64.mul dy2 dx2)

let cmp a b =
  let c = Int64.compare a.x b.x in
  if c <> 0 then c else Int64.compare a.y b.y

let graham_scan pts_arr =
  Array.sort cmp pts_arr;
  let pts = ref [] in
  for i = 0 to Array.length pts_arr - 1 do
    match !pts with
    | prev :: _ when prev.x = pts_arr.(i).x && prev.y = pts_arr.(i).y -> ()
    | _ -> pts := pts_arr.(i) :: !pts
  done;
  let pts = Array.of_list (List.rev !pts) in
  let n = Array.length pts in
  if n <= 2 then Array.to_list pts
  else begin
    let lower = ref [] in
    for i = 0 to n - 1 do
      let p = pts.(i) in
      let rec pop_lower () =
        match !lower with
        | p2 :: p1 :: rest when cross p1 p2 p <= 0L ->
          lower := p1 :: rest;
          pop_lower ()
        | _ -> ()
      in
      pop_lower ();
      lower := p :: !lower
    done;

    let upper = ref [] in
    for i = n - 1 downto 0 do
      let p = pts.(i) in
      let rec pop_upper () =
        match !upper with
        | p2 :: p1 :: rest when cross p1 p2 p <= 0L ->
          upper := p1 :: rest;
          pop_upper ()
        | _ -> ()
      in
      pop_upper ();
      upper := p :: !upper
    done;

    let lower = List.rev !lower in
    let upper = List.rev !upper in
    let lower_trim = match List.rev lower with [] -> [] | _ :: rest -> List.rev rest in
    let upper_trim = match List.rev upper with [] -> [] | _ :: rest -> List.rev rest in
    lower_trim @ upper_trim
  end

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
    let pts = Array.init n (fun _ ->
      let x = next_int64 () in
      let y = next_int64 () in
      { x; y }
    ) in
    let hull = graham_scan pts in
    let hull_arr = Array.of_list hull in
    let m = Array.length hull_arr in
    if m > 0 then begin
      let min_idx = ref 0 in
      for i = 1 to m - 1 do
        if cmp hull_arr.(i) hull_arr.(!min_idx) < 0 then min_idx := i
      done;
      let rotated = Array.init m (fun i -> hull_arr.((!min_idx + i) mod m)) in
      Printf.printf "%d\n" m;
      for i = 0 to m - 1 do
        Printf.printf "%Ld %Ld\n" rotated.(i).x rotated.(i).y
      done
    end else
      print_endline "0"
  end
