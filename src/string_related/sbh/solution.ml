(* Sequencing by hybridization: Eulerian reconstruction (Hierholzer). *)

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

module SM = Map.Make (String)

let () =
  match split_ws (read_all ()) with
  | _ :: _ :: kmers ->
      let adj = ref SM.empty and deg = ref SM.empty in
      List.iter
        (fun km ->
          let u = String.sub km 0 (String.length km - 1) in
          let v = String.sub km 1 (String.length km - 1) in
          let es = try SM.find u !adj with Not_found -> [] in
          adj := SM.add u (km :: es) !adj;
          let du = try SM.find u !deg with Not_found -> 0 in
          let dv = try SM.find v !deg with Not_found -> 0 in
          deg := SM.add u (du + 1) !deg;
          deg := SM.add v (dv - 1) !deg)
        kmers;
      adj := SM.map (fun es -> List.sort (fun x y -> compare y x) es) !adj;
      let nodes = List.sort compare (List.map fst (SM.bindings !deg)) in
      let start =
        match List.find_opt (fun u -> SM.find u !deg = 1) nodes with
        | Some u -> u
        | None ->
            let outs = List.sort compare (List.map fst (SM.bindings !adj)) in
            List.hd outs
      in
      let stack = ref [ start ] and path = ref [] in
      while !stack <> [] do
        let u = List.hd !stack in
        let es = try SM.find u !adj with Not_found -> [] in
        match es with
        | e :: rest ->
            adj := SM.add u rest !adj;
            stack := String.sub e 1 (String.length e - 1) :: !stack
        | [] ->
            path := u :: !path;
            stack := List.tl !stack
      done;
      let out = Buffer.create 16 in
      (match !path with
      | h :: tl ->
          Buffer.add_string out h;
          List.iter
            (fun node -> Buffer.add_char out node.[String.length node - 1])
            tl
      | [] -> ());
      print_string (Buffer.contents out)
  | _ -> ()
