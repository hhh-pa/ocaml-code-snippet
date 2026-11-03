let insertion_sort (a : int64 array) l r =
  for i = l + 1 to r do
    let key = a.(i) in
    let j = ref (i - 1) in
    while !j >= l && a.(!j) > key do
      a.(!j + 1) <- a.(!j);
      decr j
    done;
    a.(!j + 1) <- key
  done

let median_idx a l r =
  insertion_sort a l r;
  l + (r - l) / 2

let rec bfprt_select (a : int64 array) l r k =
  if l = r then a.(l)
  else begin
    let n = r - l + 1 in
    let num_groups = (n + 4) / 5 in
    let medians = Array.init num_groups (fun i ->
      let gl = l + i * 5 in
      let gr = min (l + (i + 1) * 5 - 1) r in
      let mi = median_idx a gl gr in
      a.(mi)
    ) in
    let pivot =
      if Array.length medians = 1 then medians.(0)
      else begin
        Array.sort compare medians;
        medians.(Array.length medians / 2)
      end
    in
    let lt = ref l in
    let gt = ref r in
    let i = ref l in
    while !i <= !gt do
      if a.(!i) < pivot then begin
        let tmp = a.(!i) in
        a.(!i) <- a.(!lt);
        a.(!lt) <- tmp;
        incr i;
        incr lt
      end else if a.(!i) > pivot then begin
        let tmp = a.(!i) in
        a.(!i) <- a.(!gt);
        a.(!gt) <- tmp;
        decr gt
      end else
        incr i
    done;
    let rank_lt = !lt - l in
    let rank_eq = !gt - !lt + 1 in
    if k < rank_lt then
      bfprt_select a l (!lt - 1) k
    else if k < rank_lt + rank_eq then
      pivot
    else
      bfprt_select a (!gt + 1) r (k - rank_lt - rank_eq)
  end

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    if n = 0 then
      Printf.printf "0\n"
    else begin
      let a = Array.init n (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
      let k = Scanf.scanf " %d" (fun x -> x) - 1 in
      if k < 0 || k >= n then
        Printf.printf "0\n"
      else begin
        let ans = bfprt_select a 0 (n - 1) k in
        Printf.printf "%Ld\n" ans
      end
    end
  with End_of_file -> ()
