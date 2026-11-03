let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let k = Scanf.scanf " %d" (fun x -> x) in
    if n > 0 && k > 0 && k <= n then begin
      let a = Array.init n (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
      let max_q = Array.make n 0 in
      let max_head = ref 0 in
      let max_tail = ref 0 in

      let min_q = Array.make n 0 in
      let min_head = ref 0 in
      let min_tail = ref 0 in

      let num_windows = n - k + 1 in
      let max_out = Array.make num_windows 0L in
      let min_out = Array.make num_windows 0L in
      let out_idx = ref 0 in

      for i = 0 to n - 1 do
        while !max_head < !max_tail && max_q.(!max_head) <= i - k do
          incr max_head
        done;
        while !max_head < !max_tail && a.(max_q.(!max_tail - 1)) <= a.(i) do
          decr max_tail
        done;
        max_q.(!max_tail) <- i;
        incr max_tail;

        while !min_head < !min_tail && min_q.(!min_head) <= i - k do
          incr min_head
        done;
        while !min_head < !min_tail && a.(min_q.(!min_tail - 1)) >= a.(i) do
          decr min_tail
        done;
        min_q.(!min_tail) <- i;
        incr min_tail;

        if i >= k - 1 then begin
          max_out.(!out_idx) <- a.(max_q.(!max_head));
          min_out.(!out_idx) <- a.(min_q.(!min_head));
          incr out_idx
        end
      done;

      for i = 0 to num_windows - 1 do
        if i > 0 then print_string " ";
        Printf.printf "%Ld" max_out.(i)
      done;
      print_newline ();

      for i = 0 to num_windows - 1 do
        if i > 0 then print_string " ";
        Printf.printf "%Ld" min_out.(i)
      done;
      print_newline ()
    end
  with End_of_file -> ()
