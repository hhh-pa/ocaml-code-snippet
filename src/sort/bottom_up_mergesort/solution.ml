let bottom_up_mergesort a =
  let n = Array.length a in
  let aux = Array.make n 0 in
  let rec merge l mid r =
    let i = ref l in
    let j = ref (mid + 1) in
    for k = l to r do
      if !i > mid then begin
        aux.(k) <- a.(!j);
        incr j
      end else if !j > r then begin
        aux.(k) <- a.(!i);
        incr i
      end else if a.(!j) < a.(!i) then begin
        aux.(k) <- a.(!j);
        incr j
      end else begin
        aux.(k) <- a.(!i);
        incr i
      end
    done;
    for k = l to r do
      a.(k) <- aux.(k)
    done
  in
  let sz = ref 1 in
  while !sz < n do
    let l = ref 0 in
    while !l < n - !sz do
      let mid = !l + !sz - 1 in
      let r = min (!l + 2 * !sz - 1) (n - 1) in
      merge !l mid r;
      l := !l + 2 * !sz
    done;
    sz := !sz * 2
  done

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let a = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    bottom_up_mergesort a;
    print_string "[";
    for i = 0 to n - 1 do
      if i > 0 then print_string ",";
      print_int a.(i)
    done;
    print_string "]"
  with End_of_file -> ()
