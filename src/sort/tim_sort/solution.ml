let ins_run a lo hi =
  for i = lo + 1 to hi - 1 do
    let cur = a.(i) in
    let j = ref (i - 1) in
    while !j >= lo && a.(!j) > cur do
      a.(!j + 1) <- a.(!j);
      decr j
    done;
    a.(!j + 1) <- cur
  done

let merge_runs a lo mid hi buf =
  let i = ref lo in
  let j = ref mid in
  let k = ref lo in
  while !i < mid && !j < hi do
    if a.(!i) <= a.(!j) then begin
      buf.(!k) <- a.(!i);
      incr i
    end else begin
      buf.(!k) <- a.(!j);
      incr j
    end;
    incr k
  done;
  while !i < mid do
    buf.(!k) <- a.(!i);
    incr i;
    incr k
  done;
  while !j < hi do
    buf.(!k) <- a.(!j);
    incr j;
    incr k
  done;
  for t = lo to hi - 1 do
    a.(t) <- buf.(t)
  done

let tim_sort a =
  let n = Array.length a in
  if n >= 2 then begin
    let min_run = 32 in
    let buf = Array.make n 0 in
    let lo = ref 0 in
    while !lo < n do
      let hi = min (!lo + min_run) n in
      ins_run a !lo hi;
      lo := !lo + min_run
    done;
    let sz = ref min_run in
    while !sz < n do
      let lo = ref 0 in
      while !lo < n - !sz do
        let mid = !lo + !sz in
        let hi = min (!lo + 2 * !sz) n in
        if a.(mid - 1) > a.(mid) then
          merge_runs a !lo mid hi buf;
        lo := !lo + 2 * !sz
      done;
      sz := !sz * 2
    done
  end

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let nums = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    tim_sort nums;
    print_string "[";
    for i = 0 to n - 1 do
      if i > 0 then print_string ",";
      print_int nums.(i)
    done;
    print_string "]"
  with End_of_file -> ()
