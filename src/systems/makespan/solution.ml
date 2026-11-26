(* Multiprocessor makespan: list scheduling + LPT rule (CLRS 35). *)

module T = Set.Make (struct
  type t = int * int

  let compare = compare
end)

let ms_schedule m jobs =
  let heap = ref T.empty in
  for i = 0 to m - 1 do
    heap := T.add (0, i) !heap
  done;
  let best = ref 0 in
  List.iter
    (fun p ->
      let load, i = T.min_elt !heap in
      heap := T.remove (load, i) !heap;
      let nl = load + p in
      best := max !best nl;
      heap := T.add (nl, i) !heap)
    jobs;
  !best

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
      let m = nums.(0) and n = nums.(1) in
      let jobs = Array.to_list (Array.sub nums 2 n) in
      let first = ms_schedule m jobs in
      let lpt = ms_schedule m (List.sort (fun a b -> compare b a) jobs) in
      Printf.printf "%d\n%d" first lpt
