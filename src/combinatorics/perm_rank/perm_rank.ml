let fact n =
  let f = ref 1L in
  for i = 2 to n do
    f := Int64.mul !f (Int64.of_int i)
  done;
  !f

let rec remove_at k = function
  | [] -> failwith "empty"
  | x :: xs ->
    if k = 0 then (x, xs)
    else
      let (v, rest) = remove_at (k - 1) xs in
      (v, x :: rest)

let () =
  try
    let mode = Scanf.scanf " %s" (fun s -> s) in
    if mode = "R" then begin
      let n = Scanf.scanf " %d" (fun x -> x) in
      let p = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
      let l_arr = Array.make n 0 in
      let rank = ref 0L in
      for i = 0 to n - 1 do
        for j = i + 1 to n - 1 do
          if p.(j) < p.(i) then l_arr.(i) <- l_arr.(i) + 1
        done;
        rank := Int64.add !rank (Int64.mul (Int64.of_int l_arr.(i)) (fact (n - 1 - i)))
      done;
      Printf.printf "%Ld\n" !rank;
      for i = 0 to n - 1 do
        if i > 0 then print_char ' ';
        print_int l_arr.(i)
      done;
      print_newline ()
    end else begin
      let n = Scanf.scanf " %d" (fun x -> x) in
      let r = ref (Scanf.scanf " %Ld" (fun x -> x)) in
      let avail = ref (List.init n (fun i -> i)) in
      let p = ref [] in
      for i = 0 to n - 1 do
        let f = fact (n - 1 - i) in
        let idx = Int64.to_int (Int64.div !r f) in
        r := Int64.rem !r f;
        let (v, rest) = remove_at idx !avail in
        avail := rest;
        p := v :: !p
      done;
      let p = List.rev !p in
      List.iteri (fun i v ->
        if i > 0 then print_char ' ';
        print_int v
      ) p;
      print_newline ()
    end
  with End_of_file -> ()
