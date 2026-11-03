let rec p_merge (a : int64 array) p1 r1 p2 r2 (b : int64 array) p3 =
  let n1 = r1 - p1 + 1 in
  let n2 = r2 - p2 + 1 in
  if n1 < n2 then
    p_merge a p2 r2 p1 r1 b p3
  else if n1 <= 0 then ()
  else if n2 <= 0 then begin
    for i = 0 to n1 - 1 do
      b.(p3 + i) <- a.(p1 + i)
    done
  end else if n1 + n2 <= 64 then begin
    let i = ref p1 in
    let j = ref p2 in
    let k = ref p3 in
    while !i <= r1 && !j <= r2 do
      if a.(!i) <= a.(!j) then begin
        b.(!k) <- a.(!i);
        incr i
      end else begin
        b.(!k) <- a.(!j);
        incr j
      end;
      incr k
    done;
    while !i <= r1 do
      b.(!k) <- a.(!i);
      incr i;
      incr k
    done;
    while !j <= r2 do
      b.(!k) <- a.(!j);
      incr j;
      incr k
    done
  end else begin
    let q1 = (p1 + r1) / 2 in
    let lo = ref p2 in
    let hi = ref r2 in
    let q2 = ref (r2 + 1) in
    while !lo <= !hi do
      let mid = (!lo + !hi) / 2 in
      if a.(mid) < a.(q1) then
        lo := mid + 1
      else begin
        q2 := mid;
        hi := mid - 1
      end
    done;
    let q3 = p3 + (q1 - p1) + (!q2 - p2) in
    b.(q3) <- a.(q1);
    p_merge a p1 (q1 - 1) p2 (!q2 - 1) b p3;
    p_merge a (q1 + 1) r1 !q2 r2 b (q3 + 1)
  end

let rec p_merge_sort (a : int64 array) p r (b : int64 array) s =
  let n = r - p + 1 in
  if n <= 64 then begin
    let sub = Array.sub a p n in
    Array.sort compare sub;
    for i = 0 to n - 1 do
      a.(p + i) <- sub.(i);
      b.(s + i) <- sub.(i)
    done
  end else begin
    let q = (p + r) / 2 in
    let q_prime = s + (q - p + 1) in
    p_merge_sort a p q b s;
    p_merge_sort a (q + 1) r b q_prime;
    p_merge a p q (q + 1) r b s;
    for i = 0 to n - 1 do
      a.(p + i) <- b.(s + i)
    done
  end

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    if n = 0 then
      Printf.printf "[]\n"
    else begin
      let a = Array.init n (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
      let aux = Array.make n 0L in
      p_merge_sort a 0 (n - 1) aux 0;
      print_string "[";
      for i = 0 to n - 1 do
        if i > 0 then print_string ",";
        Printf.printf "%Ld" a.(i)
      done;
      Printf.printf "]\n"
    end
  with End_of_file -> ()
