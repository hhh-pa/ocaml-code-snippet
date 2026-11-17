(* fractional_cascading: Berg 5.6 nested-catalog chain search. *)

let fc_lower a v =
  let lo = ref 0 and hi = ref (Array.length a) in
  while !lo < !hi do
    let mid = !lo + ((!hi - !lo) / 2) in
    if a.(mid) < v then lo := mid + 1 else hi := mid
  done;
  !lo

let fc_bridge_row cur nxt =
  let row = Array.make (Array.length cur + 1) 0 in
  Array.iteri (fun j v -> row.(j) <- fc_lower nxt v) cur;
  row.(Array.length cur) <- Array.length nxt;
  row

let fc_answer cats bridges yl yh =
  let lines = ref [] in
  let starts = Array.make (Array.length cats) 0 in
  Array.iteri
    (fun i c ->
      let p =
        if i = 0 then fc_lower cats.(0) yl else bridges.(i - 1).(starts.(i - 1))
      in
      starts.(i) <- p;
      let rep = ref [] in
      let q = ref p in
      while !q < Array.length c && c.(!q) <= yh do
        rep := c.(!q) :: !rep;
        incr q
      done;
      let r = List.rev !rep in
      lines :=
        (if r = [] then "0"
         else
           string_of_int (List.length r)
           ^ " "
           ^ String.concat " " (List.map string_of_int r))
        :: !lines)
    cats;
  String.concat "\n" (List.rev !lines)

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
      let k = nums.(0) in
      let cats = Array.make k [||] in
      let pos = ref 1 in
      for i = 0 to k - 1 do
        let m = nums.(!pos) in
        incr pos;
        cats.(i) <- Array.init m (fun j -> nums.(!pos + j));
        pos := !pos + m
      done;
      let bridges =
        Array.init
          (max 0 (k - 1))
          (fun i -> fc_bridge_row cats.(i) cats.(i + 1))
      in
      let qn = if !pos < Array.length nums then nums.(!pos) else 0 in
      incr pos;
      let buf = Buffer.create 256 in
      let first = ref true in
      for j = 0 to qn - 1 do
        let yl = nums.(!pos) and yh = nums.(!pos + 1) in
        pos := !pos + 2;
        if k > 0 then begin
          if !first then first := false else Buffer.add_char buf '\n';
          Buffer.add_string buf (fc_answer cats bridges yl yh)
        end
      done;
      print_string (Buffer.contents buf)
