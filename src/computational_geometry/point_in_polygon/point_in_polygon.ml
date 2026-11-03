type point = { x : int64; y : int64 }

let cross a b p =
  let dx1 = Int64.sub b.x a.x in
  let dy1 = Int64.sub p.y a.y in
  let dy2 = Int64.sub b.y a.y in
  let dx2 = Int64.sub p.x a.x in
  Int64.sub (Int64.mul dx1 dy1) (Int64.mul dy2 dx2)

let on_segment a b p =
  if cross a b p <> 0L then false
  else
    let min_x = min a.x b.x in
    let max_x = max a.x b.x in
    let min_y = min a.y b.y in
    let max_y = max a.y b.y in
    min_x <= p.x && p.x <= max_x && min_y <= p.y && p.y <= max_y

let classify_point poly p =
  let n = Array.length poly in
  let winding = ref 0 in
  let on_b = ref false in
  let i = ref 0 in
  while !i < n && not !on_b do
    let a = poly.(!i) in
    let b = poly.((!i + 1) mod n) in
    if on_segment a b p then on_b := true
    else begin
      if a.y <= p.y then begin
        if b.y > p.y && cross a b p > 0L then incr winding
      end else begin
        if b.y <= p.y && cross a b p < 0L then decr winding
      end;
      incr i
    end
  done;
  if !on_b then "BOUNDARY"
  else if !winding <> 0 then "INSIDE"
  else "OUTSIDE"

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
    let n = next_int () in
    let q = next_int () in
    if n >= 3 then begin
      let poly = Array.init n (fun _ ->
        let x = next_int64 () in
        let y = next_int64 () in
        { x; y }
      ) in
      for _ = 0 to q - 1 do
        if !pos < Array.length tokens then begin
          let x = next_int64 () in
          let y = next_int64 () in
          print_endline (classify_point poly { x; y })
        end
      done
    end
  end
