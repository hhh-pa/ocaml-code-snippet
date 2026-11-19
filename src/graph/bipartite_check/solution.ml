(* https://leetcode.com/problems/is-graph-bipartite/ *)
let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    if n >= 0 && m >= 0 then begin
      let adj = Array.make n [] in
      let self_loop = ref false in
      for _ = 1 to m do
        let u = Scanf.scanf " %d" (fun x -> x) in
        let v = Scanf.scanf " %d" (fun x -> x) in
        if u = v then self_loop := true
        else if u >= 0 && u < n && v >= 0 && v < n then begin
          adj.(u) <- v :: adj.(u);
          adj.(v) <- u :: adj.(v)
        end
      done;
      let color = Array.make n (-1) in
      let ok = ref true in
      for s = 0 to n - 1 do
        if color.(s) = -1 then begin
          color.(s) <- 0;
          let stack = Stack.create () in
          Stack.push s stack;
          while (not (Stack.is_empty stack)) && !ok do
            let u = Stack.pop stack in
            List.iter
              (fun w ->
                if color.(w) = -1 then begin
                  color.(w) <- 1 - color.(u);
                  Stack.push w stack
                end
                else if color.(w) = color.(u) then ok := false)
              adj.(u)
          done
        end
      done;
      print_int (if !self_loop || not !ok then 0 else 1)
    end
  with End_of_file -> ()
