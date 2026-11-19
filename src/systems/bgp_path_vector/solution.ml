(* BGP path-vector routing simulation (Tanenbaum 5). *)

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

let better cand_len frm cur =
  match cur with
  | None -> true
  | Some (nh, path) ->
      cand_len < List.length path || (cand_len = List.length path && frm < nh)

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let nums = Array.of_list (List.map int_of_string toks) in
      let n = nums.(0) and m = nums.(1) in
      let adj = Array.make n [] in
      for i = 0 to m - 1 do
        let a = nums.(2 + (2 * i)) and b = nums.(3 + (2 * i)) in
        adj.(a) <- adj.(a) @ [ b ];
        adj.(b) <- adj.(b) @ [ a ]
      done;
      let origin = nums.(2 + (2 * m)) in
      let rib = Array.make n None in
      rib.(origin) <- Some (origin, [ origin ]);
      let disc = Array.make n 0 in
      let round = ref 0 in
      let changed = ref true in
      while !changed && !round < n * n do
        incr round;
        changed := false;
        let adv = ref [] in
        for a = 0 to n - 1 do
          match rib.(a) with
          | None -> ()
          | Some _ -> List.iter (fun f -> adv := (f, a) :: !adv) adj.(a)
        done;
        List.iter
          (fun (to_, frm) ->
            let _, path =
              match rib.(frm) with Some v -> v | None -> assert false
            in
            if List.mem to_ path then disc.(to_) <- disc.(to_) + 1
            else if better (List.length path + 1) frm rib.(to_) then begin
              rib.(to_) <- Some (frm, to_ :: path);
              changed := true
            end)
          (List.rev !adv)
      done;
      let out = Buffer.create 256 in
      for a = 0 to n - 1 do
        if a = origin then
          Buffer.add_string out (Printf.sprintf "ORIGIN %d\n" origin)
        else
          match rib.(a) with
          | None -> Buffer.add_string out "UNREACHABLE\n"
          | Some (nh, path) ->
              Buffer.add_string out (string_of_int nh);
              List.iter
                (fun x -> Buffer.add_string out (" " ^ string_of_int x))
                path;
              Buffer.add_char out '\n'
      done;
      Buffer.add_string out "DISCARDS";
      for a = 0 to n - 1 do
        Buffer.add_string out (" " ^ string_of_int disc.(a))
      done;
      print_string (Buffer.contents out)
