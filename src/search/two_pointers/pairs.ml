let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let k = Scanf.scanf " %d" (fun x -> x) in
    let arr = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    Array.sort compare arr;
    let count = ref 0 in
    let i = ref 0 in
    let j = ref 1 in
    while !j < n do
      let diff = arr.(!j) - arr.(!i) in
      if diff = k then begin
        incr count;
        incr j
      end else if diff < k then
        incr j
      else begin
        incr i;
        if !i = !j then incr j
      end
    done;
    Printf.printf "%d\n" !count
  with End_of_file -> ()
