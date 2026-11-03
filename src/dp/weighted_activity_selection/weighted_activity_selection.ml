let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let s = Array.make n 0L in
    let f = Array.make n 0L in
    let w = Array.make n 0L in
    for i = 0 to n - 1 do
      s.(i) <- Scanf.scanf " %Ld" (fun x -> x);
      f.(i) <- Scanf.scanf " %Ld" (fun x -> x);
      w.(i) <- Scanf.scanf " %Ld" (fun x -> x)
    done;
    let o = Array.init n (fun i -> i) in
    Array.sort (fun a b ->
      if f.(a) <> f.(b) then
        if f.(a) < f.(b) then -1 else 1
      else if s.(a) <> s.(b) then
        if s.(a) < s.(b) then -1 else 1
      else
        compare a b
    ) o;
    let p = Array.make n (-1) in
    for j = 0 to n - 1 do
      let lo = ref 0 in
      let hi = ref (j - 1) in
      let ans = ref (-1) in
      while !lo <= !hi do
        let m = (!lo + !hi) / 2 in
        if f.(o.(m)) <= s.(o.(j)) then begin
          ans := m;
          lo := m + 1
        end else
          hi := m - 1
      done;
      p.(j) <- !ans
    done;
    let m_table = Array.make n 0L in
    for j = 0 to n - 1 do
      let incl = Int64.add w.(o.(j)) (if p.(j) >= 0 then m_table.(p.(j)) else 0L) in
      let excl = if j > 0 then m_table.(j - 1) else 0L in
      m_table.(j) <- if incl > excl then incl else excl
    done;
    let sel = ref [] in
    let j = ref (n - 1) in
    while !j >= 0 do
      let incl = Int64.add w.(o.(!j)) (if p.(!j) >= 0 then m_table.(p.(!j)) else 0L) in
      let excl = if !j > 0 then m_table.(!j - 1) else 0L in
      if incl > excl then begin
        sel := o.(!j) :: !sel;
        j := p.(!j)
      end else
        decr j
    done;
    let sel_sorted = List.sort compare !sel in
    Printf.printf "%Ld\n" (if n > 0 then m_table.(n - 1) else 0L);
    let first = ref true in
    List.iter (fun idx ->
      if not !first then print_string " ";
      first := false;
      print_int idx
    ) sel_sorted;
    print_newline ()
  with End_of_file -> ()
