let inf = 1000000000000000000L

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let words = Array.init n (fun _ -> Scanf.scanf " %s" (fun s -> s)) in
    let len = Array.map String.length words in
    let cost = Array.make (n + 1) inf in
    cost.(n) <- 0L;
    for i = n - 1 downto 0 do
      let ll = ref 0 in
      let best = ref inf in
      let break_loop = ref false in
      for j = i to n - 1 do
        if not !break_loop then begin
          ll := !ll + len.(j) + (if j > i then 1 else 0);
          if !ll > m then
            break_loop := true
          else begin
            let c =
              if j = n - 1 then 0L
              else
                let rem = Int64.of_int (m - !ll) in
                Int64.mul (Int64.mul rem rem) rem
            in
            let cand = Int64.add c cost.(j + 1) in
            if cand < !best then best := cand
          end
        end
      done;
      cost.(i) <- !best
    done;
    Printf.printf "%Ld\n" cost.(0)
  with End_of_file -> ()
