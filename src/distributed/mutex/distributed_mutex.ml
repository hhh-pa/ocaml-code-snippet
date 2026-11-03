let () =
  try
    let mode = Scanf.scanf " %s" (fun x -> x) in
    let n = Scanf.scanf " %d" (fun x -> x) in
    if mode = "ra" then begin
      let rounds = Scanf.scanf " %d" (fun x -> x) in
      let order = ref [] in
      for _ = 0 to rounds - 1 do
        let c = Scanf.scanf " %d" (fun x -> x) in
        let g = Array.init c (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
        Array.sort compare g;
        for j = 0 to c - 1 do
          order := g.(j) :: !order
        done
      done;
      let order_list = List.rev !order in
      Printf.printf "ORDER:";
      List.iter (fun x -> Printf.printf " %d" x) order_list;
      let msgs = Int64.mul (Int64.mul 2L (Int64.of_int (n - 1))) (Int64.of_int (List.length order_list)) in
      Printf.printf "\nMSGS: %Ld\n" msgs
    end else begin
      let holder = ref (Scanf.scanf " %d" (fun x -> x)) in
      let q_count = Scanf.scanf " %d" (fun x -> x) in
      let req = Array.init q_count (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
      let order = ref [] in
      let ln = Array.make n 0L in
      let rn = Array.init n (fun _ -> Array.make n 0L) in
      let q = Queue.create () in
      let hops = ref 0L in
      for iq = 0 to q_count - 1 do
        let i = req.(iq) in
        rn.(i).(i) <- Int64.add rn.(i).(i) 1L;
        for t = 0 to n - 1 do
          rn.(t).(i) <- rn.(i).(i)
        done;
        if i = !holder then begin
          order := i :: !order;
          ln.(i) <- rn.(i).(i)
        end else begin
          Queue.push i q;
          while not (Queue.is_empty q) do
            let j = Queue.pop q in
            if rn.(!holder).(j) > ln.(j) then begin
              ln.(j) <- rn.(!holder).(j);
              for t = 0 to n - 1 do
                rn.(j).(t) <- max rn.(j).(t) rn.(!holder).(t)
              done;
              holder := j;
              hops := Int64.add !hops 1L;
              order := j :: !order
            end
          done
        end
      done;
      let order_list = List.rev !order in
      Printf.printf "ORDER:";
      List.iter (fun x -> Printf.printf " %d" x) order_list;
      let msgs = Int64.add (Int64.mul (Int64.of_int (n - 1)) (Int64.of_int q_count)) !hops in
      Printf.printf "\nHOPS: %Ld\n" !hops;
      Printf.printf "MSGS: %Ld\n" msgs
    end
  with End_of_file -> ()
