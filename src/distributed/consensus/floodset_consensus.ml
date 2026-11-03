module IntSet = Set.Make(Int)

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let f = Scanf.scanf " %d" (fun x -> x) in
    let init = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    let nf = Scanf.scanf " %d" (fun x -> x) in
    let faulty = Array.make n false in
    for _ = 0 to nf - 1 do
      let id = Scanf.scanf " %d" (fun x -> x) in
      faulty.(id) <- true
    done;
    let w = Array.init n (fun i -> IntSet.singleton init.(i)) in
    for _ = 0 to f do
      let nw = Array.copy w in
      for i = 0 to n - 1 do
        if not faulty.(i) then begin
          for j = 0 to n - 1 do
            if not faulty.(j) && i <> j then
              nw.(i) <- IntSet.union nw.(i) w.(j)
          done
        end
      done;
      Array.blit nw 0 w 0 n
    done;
    for i = 0 to n - 1 do
      if not faulty.(i) then
        Printf.printf "P%d: %d\n" i (IntSet.min_elt w.(i))
    done
  with End_of_file -> ()
