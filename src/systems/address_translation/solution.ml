(* Page-table walk address translation (CS:APP ch 9). *)

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

let prefix_match vpns idx level =
  let rec go i =
    if i >= level then true
    else if vpns.(i) <> idx.(i) then false
    else go (i + 1)
  in
  go 0

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let t = Array.of_list (List.map int_of_string toks) in
      let pos = ref 0 in
      let next () =
        let v = t.(!pos) in
        incr pos;
        v
      in
      let w = next () and m = next () and k = next () in
      let nt = next () in
      let paths = ref [] in
      for _ = 1 to nt do
        let vpns = Array.init k (fun _ -> next ()) in
        let ppn = next () in
        paths := (vpns, ppn) :: !paths
      done;
      let paths = List.rev !paths in
      let nq = next () in
      let out = Buffer.create 256 in
      for _ = 1 to nq do
        let va = next () in
        if va >= 1 lsl ((k * w) + m) then Buffer.add_string out "FAULT 0\n"
        else
          let vpn = va lsr m in
          let mask = (1 lsl w) - 1 in
          let idx =
            Array.init k (fun i -> (vpn lsr (w * (k - 1 - i))) land mask)
          in
          let flt = ref 0 in
          let lv = ref 1 in
          while !flt = 0 && !lv <= k do
            if List.exists (fun (vpns, _) -> prefix_match vpns idx !lv) paths
            then incr lv
            else flt := !lv
          done;
          if !flt <> 0 then
            Buffer.add_string out (Printf.sprintf "FAULT %d\n" !flt)
          else
            let ppn = ref (-1) in
            List.iter
              (fun (vpns, p) -> if prefix_match vpns idx k then ppn := p)
              paths;
            if !ppn < 0 then
              Buffer.add_string out (Printf.sprintf "FAULT %d\n" k)
            else
              let pa = (!ppn lsl m) lor (va land ((1 lsl m) - 1)) in
              Buffer.add_string out (Printf.sprintf "PA %d\n" pa)
      done;
      print_string (Buffer.contents out)
