let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    if n >= 0 then begin
      let h = Array.init n (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
      let max_area = ref 0L in
      let st = Array.make (n + 1) 0 in
      let top = ref 0 in

      for i = 0 to n do
        let cur_h = if i = n then -1L else h.(i) in
        while !top > 0 && h.(st.(!top - 1)) >= cur_h do
          decr top;
          let height = h.(st.(!top)) in
          let width =
            if !top = 0 then Int64.of_int i
            else Int64.of_int (i - st.(!top - 1) - 1)
          in
          let area = Int64.mul height width in
          if area > !max_area then max_area := area
        done;
        st.(!top) <- i;
        incr top
      done;

      let span = Array.make n 0 in
      top := 0;
      for i = 0 to n - 1 do
        while !top > 0 && h.(st.(!top - 1)) <= h.(i) do
          decr top
        done;
        span.(i) <- (if !top = 0 then i + 1 else i - st.(!top - 1));
        st.(!top) <- i;
        incr top
      done;

      Printf.printf "%Ld\n" !max_area;
      for i = 0 to n - 1 do
        if i > 0 then print_string " ";
        Printf.printf "%d" span.(i)
      done;
      print_newline ()
    end
  with End_of_file -> ()
