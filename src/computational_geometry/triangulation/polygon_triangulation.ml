(* Polygon triangulation by ear clipping *)

type point = { x : float; y : float; id : int }

let eps = 1e-9

let cross a b c =
  (b.x -. a.x) *. (c.y -. a.y) -. (b.y -. a.y) *. (c.x -. a.x)

let point_in_triangle p a b c =
  let cp1 = cross a b p in
  let cp2 = cross b c p in
  let cp3 = cross c a p in
  let has_neg = cp1 < -.eps || cp2 < -.eps || cp3 < -.eps in
  let has_pos = cp1 > eps || cp2 > eps || cp3 > eps in
  not (has_neg && has_pos)

let rec remove_at i = function
  | [] -> []
  | x :: xs -> if i = 0 then xs else x :: remove_at (i - 1) xs

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
    let poly_init = Array.init n (fun i ->
      let x = float_of_string (next_token ()) in
      let y = float_of_string (next_token ()) in
      { x; y; id = i }
    ) in
    let signed_area = ref 0.0 in
    for i = 0 to n - 1 do
      let next_i = (i + 1) mod n in
      signed_area := !signed_area +. (poly_init.(i).x *. poly_init.(next_i).y -. poly_init.(next_i).x *. poly_init.(i).y)
    done;
    let poly_list =
      if !signed_area < 0.0 then
        List.rev (Array.to_list poly_init)
      else
        Array.to_list poly_init
    in
    let poly = ref poly_list in
    let triangles = ref [] in
    let finished = ref false in
    while List.length !poly > 3 && not !finished do
      let sz = List.length !poly in
      let poly_arr = Array.of_list !poly in
      let ear_found = ref false in
      let i = ref 0 in
      while !i < sz && not !ear_found do
        let prev_i = (!i + sz - 1) mod sz in
        let next_i = (!i + 1) mod sz in
        let a = poly_arr.(prev_i) in
        let b = poly_arr.(!i) in
        let c = poly_arr.(next_i) in
        if cross a b c > eps then begin
          let is_ear = ref true in
          let j = ref 0 in
          while !j < sz && !is_ear do
            if !j <> prev_i && !j <> !i && !j <> next_i then begin
              if point_in_triangle poly_arr.(!j) a b c then
                is_ear := false
            end;
            incr j
          done;
          if !is_ear then begin
            let tri = List.sort compare [a.id; b.id; c.id] in
            (match tri with
             | [x; y; z] -> triangles := (x, y, z) :: !triangles
             | _ -> ());
            poly := remove_at !i !poly;
            ear_found := true
          end
        end;
        incr i
      done;
      if not !ear_found then finished := true
    done;
    if List.length !poly = 3 then begin
      let poly_arr = Array.of_list !poly in
      let tri = List.sort compare [poly_arr.(0).id; poly_arr.(1).id; poly_arr.(2).id] in
      (match tri with
       | [x; y; z] -> triangles := (x, y, z) :: !triangles
       | _ -> ())
    end;
    let tri_arr = Array.of_list !triangles in
    Array.sort (fun (i1, j1, k1) (i2, j2, k2) ->
      if i1 <> i2 then compare i1 i2
      else if j1 <> j2 then compare j1 j2
      else compare k1 k2
    ) tri_arr;
    Printf.printf "%d\n" (Array.length tri_arr);
    Array.iter (fun (i, j, k) ->
      Printf.printf "%d %d %d\n" i j k
    ) tri_arr
  with End_of_file -> ()
