type pt = { x : int64; y : int64; z : int64 }

let orient3 a b c d =
  let abx = Int64.sub b.x a.x in
  let aby = Int64.sub b.y a.y in
  let abz = Int64.sub b.z a.z in
  let acx = Int64.sub c.x a.x in
  let acy = Int64.sub c.y a.y in
  let acz = Int64.sub c.z a.z in
  let adx = Int64.sub d.x a.x in
  let ady = Int64.sub d.y a.y in
  let adz = Int64.sub d.z a.z in
  let cx = Int64.sub (Int64.mul aby acz) (Int64.mul abz acy) in
  let cy = Int64.sub (Int64.mul abz acx) (Int64.mul abx acz) in
  let cz = Int64.sub (Int64.mul abx acy) (Int64.mul aby acx) in
  Int64.add
    (Int64.mul cx adx)
    (Int64.add (Int64.mul cy ady) (Int64.mul cz adz))

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
      let z = next_int64 () in
      { x; y; z }
    ) in

    let ord = Array.init n (fun i -> i) in
    let st = ref 199L in
    let a_const = 6364136223846793005L in
    let c_const = 1442695040888963407L in
    for i = n - 1 downto 1 do
      st := Int64.add (Int64.mul !st a_const) c_const;
      let shifted = Int64.shift_right_logical !st 33 in
      let j = Int64.to_int (Int64.rem shifted (Int64.of_int (i + 1))) in
      let tmp = ord.(i) in
      ord.(i) <- ord.(j);
      ord.(j) <- tmp
    done;

    let p0 = ord.(0) in
    let p1 = ord.(1) in
    let p2 = ord.(2) in
    let p3 = ord.(3) in
    let t = [| p0; p1; p2; p3 |] in

    let facets = ref [] in
    for skip = 0 to 3 do
      let v = ref [] in
      for i = 0 to 3 do
        if i <> skip then v := t.(i) :: !v
      done;
      let varr = Array.of_list (List.rev !v) in
      if orient3 pts.(varr.(0)) pts.(varr.(1)) pts.(varr.(2)) pts.(t.(skip)) > 0L then begin
        let tmp = varr.(1) in
        varr.(1) <- varr.(2);
        varr.(2) <- tmp
      end;
      facets := (varr.(0), varr.(1), varr.(2)) :: !facets
    done;

    let orient_inner a b c =
      let o0 = orient3 pts.(a) pts.(b) pts.(c) pts.(p0) in
      let o1 = orient3 pts.(a) pts.(b) pts.(c) pts.(p1) in
      let o2 = orient3 pts.(a) pts.(b) pts.(c) pts.(p2) in
      let o3 = orient3 pts.(a) pts.(b) pts.(c) pts.(p3) in
      Int64.add (Int64.add o0 o1) (Int64.add o2 o3)
    in

    for oi = 4 to n - 1 do
      let p = ord.(oi) in
      let cur_facets = Array.of_list !facets in
      let vis = Array.make (Array.length cur_facets) false in
      let has_vis = ref false in
      for f = 0 to Array.length cur_facets - 1 do
        let (fa, fb, fc) = cur_facets.(f) in
        if orient3 pts.(fa) pts.(fb) pts.(fc) pts.(p) > 0L then begin
          vis.(f) <- true;
          has_vis := true
        end
      done;
      if !has_vis then begin
        let ecount = Hashtbl.create 32 in
        for f = 0 to Array.length cur_facets - 1 do
          if vis.(f) then begin
            let (fa, fb, fc) = cur_facets.(f) in
            let vs = [| fa; fb; fc |] in
            for e = 0 to 2 do
              let u = vs.(e) in
              let v = vs.((e + 1) mod 3) in
              let (mn, mx) = if u < v then (u, v) else (v, u) in
              let prev = match Hashtbl.find_opt ecount (mn, mx) with Some c -> c | None -> 0 in
              Hashtbl.replace ecount (mn, mx) (prev + 1)
            done
          end
        done;
        let nf = ref [] in
        for f = 0 to Array.length cur_facets - 1 do
          if not vis.(f) then nf := cur_facets.(f) :: !nf
        done;
        let edges = ref [] in
        Hashtbl.iter (fun edge count ->
          if count = 1 then edges := edge :: !edges
        ) ecount;
        let edges = List.sort compare !edges in
        List.iter (fun (u, v) ->
          let (a, b) =
            if orient_inner u v p > 0L then (v, u)
            else (u, v)
          in
          nf := (a, b, p) :: !nf
        ) edges;
        facets := !nf
      end
    done;

    let out = List.map (fun (a, b, c) ->
      let arr = [| a; b; c |] in
      Array.sort compare arr;
      (arr.(0), arr.(1), arr.(2))
    ) !facets in
    let out = List.sort compare out in
    Printf.printf "%d\n" (List.length out);
    List.iter (fun (a, b, c) ->
      Printf.printf "%d %d %d\n" a b c
    ) out
  end
