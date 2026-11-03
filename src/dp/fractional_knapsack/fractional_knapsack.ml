let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let cap = Scanf.scanf " %Ld" (fun x -> x) in
    let v = Array.make n 0L in
    let w = Array.make n 0L in
    for i = 0 to n - 1 do
      v.(i) <- Scanf.scanf " %Ld" (fun x -> x);
      w.(i) <- Scanf.scanf " %Ld" (fun x -> x)
    done;
    let order = Array.init n (fun i -> i) in
    Array.sort (fun a b ->
      let l = Int64.to_float v.(a) *. Int64.to_float w.(b) in
      let r = Int64.to_float v.(b) *. Int64.to_float w.(a) in
      if l <> r then
        if l > r then -1 else 1
      else
        compare a b
    ) order;
    let tot = ref 0.0 in
    let rem = ref (Int64.to_float cap) in
    for idx = 0 to n - 1 do
      let i = order.(idx) in
      if !rem > 0.0 then begin
        let wi = Int64.to_float w.(i) in
        let vi = Int64.to_float v.(i) in
        if wi <= !rem then begin
          tot := !tot +. vi;
          rem := !rem -. wi
        end else begin
          tot := !tot +. vi *. (!rem /. wi);
          rem := 0.0
        end
      end
    done;
    Printf.printf "%.6f\n" !tot
  with End_of_file -> ()
