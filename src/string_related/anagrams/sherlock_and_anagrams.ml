let sort_string s =
  let len = String.length s in
  let b = Bytes.of_string s in
  for i = 0 to len - 1 do
    for j = i + 1 to len - 1 do
      if Bytes.get b i > Bytes.get b j then begin
        let t = Bytes.get b i in
        Bytes.set b i (Bytes.get b j);
        Bytes.set b j t
      end
    done
  done;
  Bytes.to_string b

let solve_case () =
  let s = Scanf.scanf " %s" (fun x -> x) in
  let n = String.length s in
  let tbl = Hashtbl.create 1024 in
  for i = 0 to n - 1 do
    for j = i to n - 1 do
      let sub = String.sub s i (j - i + 1) in
      let sorted_sub = sort_string sub in
      let cur = try Hashtbl.find tbl sorted_sub with Not_found -> 0 in
      Hashtbl.replace tbl sorted_sub (cur + 1)
    done
  done;
  let ans = ref 0 in
  Hashtbl.iter (fun _ c ->
    ans := !ans + c * (c - 1) / 2
  ) tbl;
  Printf.printf "%d\n" !ans

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
