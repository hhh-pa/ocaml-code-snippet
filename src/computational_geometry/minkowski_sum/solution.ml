type pt = { x : int64; y : int64 }

let cross o a b =
  let dx1 = Int64.sub a.x o.x in
  let dy1 = Int64.sub b.y o.y in
  let dy2 = Int64.sub a.y o.y in
  let dx2 = Int64.sub b.x o.x in
  Int64.sub (Int64.mul dx1 dy1) (Int64.mul dy2 dx2)

let cmp a b =
  let c = Int64.compare a.x b.x in
  if c <> 0 then c else Int64.compare a.y b.y

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
    let big_a = Array.init n (fun _ ->
      let x = next_int64 () in
      let y = next_int64 () in
      { x; y }
    ) in
    let m = next_int () in
    let big_b = Array.init m (fun _ ->
      let x = next_int64 () in
      let y = next_int64 () in
      { x; y }
    ) in
    let raw = ref [] in
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        raw := { x = Int64.add big_a.(i).x big_b.(j).x; y = Int64.add big_a.(i).y big_b.(j).y } :: !raw
      done
    done;
    let pts = Array.of_list !raw in
    Array.sort cmp pts;
    let unique_pts = ref [] in
    for i = 0 to Array.length pts - 1 do
      match !unique_pts with
      | prev :: _ when prev.x = pts.(i).x && prev.y = pts.(i).y -> ()
      | _ -> unique_pts := pts.(i) :: !unique_pts
    done;
    let pts = Array.of_list (List.rev !unique_pts) in
    let plen = Array.length pts in

    let lo = ref [] in
    for i = 0 to plen - 1 do
      let q = pts.(i) in
      let rec pop_lo () =
        match !lo with
        | p2 :: p1 :: rest when cross p1 p2 q <= 0L ->
          lo := p1 :: rest;
          pop_lo ()
        | _ -> ()
      in
      pop_lo ();
      lo := q :: !lo
    done;

    let hi = ref [] in
    for i = plen - 1 downto 0 do
      let q = pts.(i) in
      let rec pop_hi () =
        match !hi with
        | p2 :: p1 :: rest when cross p1 p2 q <= 0L ->
          hi := p1 :: rest;
          pop_hi ()
        | _ -> ()
      in
      pop_hi ();
      hi := q :: !hi
    done;

    let lo = List.rev !lo in
    let hi = List.rev !hi in
    let lo_trim = match List.rev lo with [] -> [] | _ :: rest -> List.rev rest in
    let hi_trim = match List.rev hi with [] -> [] | _ :: rest -> List.rev rest in
    let hull = lo_trim @ hi_trim in

    Printf.printf "%d\n" (List.length hull);
    List.iter (fun p -> Printf.printf "%Ld %Ld\n" p.x p.y) hull
  end
