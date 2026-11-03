(* Fortune / Voronoi vertices via empty circumcircles *)

type point = { x : float; y : float }

let eps = 1e-9

let circumcircle a b c =
  let d = 2.0 *. (a.x *. (b.y -. c.y) +. b.x *. (c.y -. a.y) +. c.x *. (a.y -. b.y)) in
  if abs_float d < eps then None
  else
    let a2 = a.x *. a.x +. a.y *. a.y in
    let b2 = b.x *. b.x +. b.y *. b.y in
    let c2 = c.x *. c.x +. c.y *. c.y in
    let ux = (a2 *. (b.y -. c.y) +. b2 *. (c.y -. a.y) +. c2 *. (a.y -. b.y)) /. d in
    let uy = (a2 *. (c.x -. b.x) +. b2 *. (a.x -. c.x) +. c2 *. (b.x -. a.x)) /. d in
    let r_sq = (a.x -. ux) *. (a.x -. ux) +. (a.y -. uy) *. (a.y -. uy) in
    Some ({ x = ux; y = uy }, r_sq)

let point_compare a b =
  if abs_float (a.x -. b.x) > eps then
    if a.x < b.x then -1 else 1
  else if abs_float (a.y -. b.y) > eps then
    if a.y < b.y then -1 else 1
  else 0

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
    let n_str = next_token () in
    let n = int_of_string n_str in
    let pts = Array.init n (fun _ ->
      let x = float_of_string (next_token ()) in
      let y = float_of_string (next_token ()) in
      { x; y }
    ) in
    let centers = ref [] in
    for i = 0 to n - 1 do
      for j = i + 1 to n - 1 do
        for k = j + 1 to n - 1 do
          match circumcircle pts.(i) pts.(j) pts.(k) with
          | None -> ()
          | Some (center, r_sq) ->
            let valid = ref true in
            let m = ref 0 in
            while !m < n && !valid do
              if !m <> i && !m <> j && !m <> k then begin
                let dx = pts.(!m).x -. center.x in
                let dy = pts.(!m).y -. center.y in
                let dist_sq = dx *. dx +. dy *. dy in
                if dist_sq < r_sq -. eps then
                  valid := false
              end;
              incr m
            done;
            if !valid then
              centers := center :: !centers
        done
      done
    done;
    let sorted_centers = List.sort point_compare !centers in
    let unique_centers = ref [] in
    List.iter (fun c ->
      match !unique_centers with
      | [] -> unique_centers := [c]
      | prev :: _ ->
        if abs_float (c.x -. prev.x) > eps || abs_float (c.y -. prev.y) > eps then
          unique_centers := c :: !unique_centers
    ) sorted_centers;
    let result = List.rev !unique_centers in
    Printf.printf "%d\n" (List.length result);
    List.iter (fun c ->
      Printf.printf "%.2f %.2f\n" c.x c.y
    ) result
  with End_of_file -> ()
