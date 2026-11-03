let insertion_sort a lo hi =
  for i = lo + 1 to hi do
    let cur = a.(i) in
    let j = ref (i - 1) in
    while !j >= lo && a.(!j) > cur do
      a.(!j + 1) <- a.(!j);
      decr j
    done;
    a.(!j + 1) <- cur
  done

let heap_sort_range a lo hi =
  let n = hi - lo + 1 in
  let sift_down root finish_len =
    let r = ref root in
    let cont = ref true in
    while (2 * (!r - lo) + 1) < finish_len && !cont do
      let c = lo + 2 * (!r - lo) + 1 in
      let sw = ref !r in
      if a.(!sw) < a.(c) then sw := c;
      if c + 1 < lo + finish_len && a.(!sw) < a.(c + 1) then sw := c + 1;
      if !sw = !r then cont := false
      else begin
        let tmp = a.(!r) in
        a.(!r) <- a.(!sw);
        a.(!sw) <- tmp;
        r := !sw
      end
    done
  in
  for start = n / 2 - 1 downto 0 do
    sift_down (lo + start) n
  done;
  for finish = hi downto lo + 1 do
    let tmp = a.(lo) in
    a.(lo) <- a.(finish);
    a.(finish) <- tmp;
    sift_down lo (finish - lo)
  done

let intro_sort a =
  let n = Array.length a in
  if n > 1 then begin
    let rec log2 n = if n <= 1 then 0 else 1 + log2 (n / 2) in
    let max_depth = 2 * log2 n in
    let rec introsort lo hi depth =
      let cur_lo = ref lo in
      let cur_hi = ref hi in
      let cur_depth = ref depth in
      while !cur_hi - !cur_lo > 16 do
        if !cur_depth = 0 then begin
          heap_sort_range a !cur_lo !cur_hi;
          cur_lo := !cur_hi + 1
        end else begin
          decr cur_depth;
          let mid = (!cur_lo + !cur_hi) / 2 in
          if a.(mid) < a.(!cur_lo) then begin
            let t = a.(!cur_lo) in a.(!cur_lo) <- a.(mid); a.(mid) <- t
          end;
          if a.(!cur_hi) < a.(!cur_lo) then begin
            let t = a.(!cur_lo) in a.(!cur_lo) <- a.(!cur_hi); a.(!cur_hi) <- t
          end;
          if a.(!cur_hi) < a.(mid) then begin
            let t = a.(mid) in a.(mid) <- a.(!cur_hi); a.(!cur_hi) <- t
          end;
          let pivot = a.(mid) in
          let i = ref !cur_lo in
          let j = ref !cur_hi in
          while !i <= !j do
            while a.(!i) < pivot do incr i done;
            while a.(!j) > pivot do decr j done;
            if !i <= !j then begin
              let t = a.(!i) in a.(!i) <- a.(!j); a.(!j) <- t;
              incr i;
              decr j
            end
          done;
          if !j - !cur_lo < !cur_hi - !i then begin
            if !cur_lo < !j then introsort !cur_lo !j !cur_depth;
            cur_lo := !i
          end else begin
            if !i < !cur_hi then introsort !i !cur_hi !cur_depth;
            cur_hi := !j
          end
        end
      done;
      if !cur_lo < !cur_hi then insertion_sort a !cur_lo !cur_hi
    in
    introsort 0 (n - 1) max_depth
  end

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let nums = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    intro_sort nums;
    print_string "[";
    for i = 0 to n - 1 do
      if i > 0 then print_string ",";
      print_int nums.(i)
    done;
    print_string "]"
  with End_of_file -> ()
