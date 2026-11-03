let log2 x = log x /. log 2.0

module Int64ListSet = Set.Make(struct
  type t = int64 list
  let compare = compare
end)

let () =
  let tokens =
    let buf = Buffer.create 4096 in
    (try while true do Buffer.add_channel buf stdin 4096 done with End_of_file -> ());
    let s = Buffer.contents buf in
    let len = String.length s in
    let res = ref [] in
    let i = ref 0 in
    while !i < len do
      while !i < len && (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\r' || s.[!i] = '\n') do incr i done;
      if !i < len then begin
        let st = !i in
        while !i < len && not (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\r' || s.[!i] = '\n') do incr i done;
        res := String.sub s st (!i - st) :: !res
      end
    done;
    Array.of_list (List.rev !res)
  in
  if Array.length tokens >= 1 then begin
    let m = int_of_string tokens.(0) in
    let s = Array.init m (fun i -> Int64.of_string tokens.(1 + i)) in
    (* 1. Plugin *)
    let counts = Hashtbl.create 64 in
    for i = 0 to m - 1 do
      let v = s.(i) in
      let prev = try Hashtbl.find counts v with Not_found -> 0 in
      Hashtbl.replace counts v (prev + 1)
    done;
    let plug = ref 0.0 in
    Hashtbl.iter (fun _ cnt ->
      let pr = float_of_int cnt /. float_of_int m in
      plug := !plug -. pr *. log2 pr
    ) counts;
    (* 2. LZ78 *)
    let dict = ref Int64ListSet.empty in
    let c = ref 0 in
    let i = ref 0 in
    while !i < m do
      let j = ref !i in
      let cur = ref [s.(!i)] in
      while !j < m && Int64ListSet.mem (List.rev !cur) !dict do
        incr j;
        if !j < m then cur := s.(!j) :: !cur
      done;
      if !j >= m then begin
        incr c;
        i := m
      end else begin
        dict := Int64ListSet.add (List.rev !cur) !dict;
        incr c;
        i := !j + 1
      end
    done;
    let lz = float_of_int !c *. log2 (float_of_int m) /. float_of_int m in
    (* 3. Kontoyiannis *)
    let l2n = log2 (float_of_int m) in
    let ky = ref 0.0 in
    let nk = ref 0 in
    let start_p = max 1 (m / 2) in
    for p = start_p to m - 1 do
      let best = ref 0 in
      for q = 0 to p - 1 do
        let l = ref 0 in
        while p + !l < m && s.(q + !l) = s.(p + !l) do
          incr l
        done;
        if !l > !best then best := !l
      done;
      if !best >= 1 then begin
        ky := !ky +. l2n /. float_of_int !best;
        incr nk
      end
    done;
    let kyv = if !nk > 0 then !ky /. float_of_int !nk else l2n in
    Printf.printf "PLUGIN: %.6f\n" (!plug +. 0.0);
    Printf.printf "LZ: %.6f\n" (lz +. 0.0);
    Printf.printf "KY: %.6f\n" (kyv +. 0.0)
  end
