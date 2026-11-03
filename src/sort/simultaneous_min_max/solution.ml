let simultaneous_min_max (a : int64 array) =
  let n = Array.length a in
  if n = 0 then (0L, 0L)
  else if n = 1 then (a.(0), a.(0))
  else begin
    let mn = ref 0L in
    let mx = ref 0L in
    let start_i = ref 0 in
    if n mod 2 = 0 then begin
      if a.(0) < a.(1) then begin
        mn := a.(0);
        mx := a.(1)
      end else begin
        mn := a.(1);
        mx := a.(0)
      end;
      start_i := 2
    end else begin
      mn := a.(0);
      mx := a.(0);
      start_i := 1
    end;
    let i = ref !start_i in
    while !i + 1 < n do
      let sm = ref 0L in
      let lg = ref 0L in
      if a.(!i) < a.(!i + 1) then begin
        sm := a.(!i);
        lg := a.(!i + 1)
      end else begin
        sm := a.(!i + 1);
        lg := a.(!i)
      end;
      if !sm < !mn then mn := !sm;
      if !lg > !mx then mx := !lg;
      i := !i + 2
    done;
    (!mn, !mx)
  end

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    if n = 0 then
      Printf.printf "0 0\n"
    else begin
      let a = Array.init n (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
      let (mn, mx) = simultaneous_min_max a in
      Printf.printf "%Ld %Ld\n" mn mx
    end
  with End_of_file -> ()
