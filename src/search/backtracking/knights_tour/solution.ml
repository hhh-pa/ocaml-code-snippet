(* Knight's tour via Warnsdorff (ties: smallest row, then column). *)

let moves =
  [| (2, 1); (1, 2); (-1, 2); (-2, 1); (-2, -1); (-1, -2); (1, -2); (2, -1) |]

let kt_onward seen n r c =
  let cnt = ref 0 in
  Array.iter
    (fun (dr, dc) ->
      let nr = r + dr and nc = c + dc in
      if nr >= 0 && nr < n && nc >= 0 && nc < n && not seen.(nr).(nc) then
        incr cnt)
    moves;
  !cnt

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
      let n = nums.(0) and sr = nums.(1) and sc = nums.(2) in
      let seen = Array.make_matrix n n false in
      let tour = ref [ (sr, sc) ] in
      seen.(sr).(sc) <- true;
      let stuck = ref false in
      while List.length !tour < n * n && not !stuck do
        let r, c = List.hd !tour in
        let cands = ref [] in
        Array.iter
          (fun (dr, dc) ->
            let nr = r + dr and nc = c + dc in
            if nr >= 0 && nr < n && nc >= 0 && nc < n && not seen.(nr).(nc) then
              cands := (kt_onward seen n nr nc, nr, nc) :: !cands)
          moves;
        match List.sort compare !cands with
        | [] -> stuck := true
        | (_, nr, nc) :: _ ->
            seen.(nr).(nc) <- true;
            tour := (nr, nc) :: !tour
      done;
      if !stuck then print_string "NO TOUR"
      else
        let t = List.rev !tour in
        List.iteri
          (fun i (r, c) ->
            if i > 0 then print_char '\n';
            Printf.printf "%d %d" r c)
          t
