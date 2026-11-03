let holds a b op =
  if op = 0 then a <> b
  else if op = 1 then a = b
  else if op = 2 then a < b
  else a > b

let flipop op =
  if op = 2 then 3
  else if op = 3 then 2
  else op

let solve () =
  let n = Scanf.scanf " %d" (fun x -> x) in
  let dom = Array.make n [] in
  for i = 0 to n - 1 do
    let s = Scanf.scanf " %d" (fun x -> x) in
    let d = ref [] in
    for _ = 0 to s - 1 do
      d := Scanf.scanf " %d" (fun x -> x) :: !d
    done;
    dom.(i) <- List.sort compare !d
  done;
  let c = Scanf.scanf " %d" (fun x -> x) in
  let nbr = Array.make n [] in
  for _ = 0 to c - 1 do
    let u = Scanf.scanf " %d" (fun x -> x) in
    let v = Scanf.scanf " %d" (fun x -> x) in
    let op = Scanf.scanf " %d" (fun x -> x) in
    nbr.(u) <- (v, op) :: nbr.(u);
    nbr.(v) <- (u, flipop op) :: nbr.(v)
  done;
  let q = Queue.create () in
  for i = 0 to n - 1 do
    List.iter (fun (v, op) -> Queue.push (i, v, op) q) (List.rev nbr.(i))
  done;
  let ok = ref true in
  while not (Queue.is_empty q) && !ok do
    let (ai, aj, aop) = Queue.pop q in
    let nd = ref [] in
    let ch = ref false in
    List.iter (fun x ->
      let sup = List.exists (fun y -> holds x y aop) dom.(aj) in
      if sup then nd := x :: !nd
      else ch := true
    ) dom.(ai);
    let new_dom = List.rev !nd in
    if !ch then begin
      dom.(ai) <- new_dom;
      if new_dom = [] then ok := false
      else begin
        List.iter (fun (v, op) ->
          if v <> aj then Queue.push (v, ai, op) q
        ) (List.rev nbr.(ai))
      end
    end
  done;
  if not !ok then
    Printf.printf "AC3 INCONSISTENT\n"
  else begin
    Printf.printf "AC3\n";
    for i = 0 to n - 1 do
      let first = ref true in
      List.iter (fun x ->
        if not !first then Printf.printf " ";
        Printf.printf "%d" x;
        first := false
      ) dom.(i);
      Printf.printf "\n"
    done
  end

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve ()
    done
  with End_of_file -> ()
