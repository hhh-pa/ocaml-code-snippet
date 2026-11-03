type acceptor = {
  id: int;
  mutable promised_n: int;
  mutable accepted_n: int;
  mutable accepted_val: string;
}

let () =
  try
    let n_acceptors = Scanf.scanf " %d" (fun x -> x) in
    let n_ops = Scanf.scanf " %d" (fun x -> x) in
    let acceptors = Array.init n_acceptors (fun i ->
      { id = i; promised_n = 0; accepted_n = 0; accepted_val = "" }
    ) in
    let majority = n_acceptors / 2 + 1 in
    for _ = 0 to n_ops - 1 do
      let op = Scanf.scanf " %s" (fun x -> x) in
      if op = "PREPARE" then begin
        let n_prop = Scanf.scanf " %d" (fun x -> x) in
        let promises = ref 0 in
        let max_acc_n = ref 0 in
        let val_for_max = ref "" in
        for i = 0 to n_acceptors - 1 do
          let acc = acceptors.(i) in
          if n_prop > acc.promised_n then begin
            acc.promised_n <- n_prop;
            incr promises;
            if acc.accepted_n > !max_acc_n then begin
              max_acc_n := acc.accepted_n;
              val_for_max := acc.accepted_val
            end
          end
        done;
        if !promises >= majority then begin
          if !max_acc_n > 0 then
            Printf.printf "PROMISED %d PREV %d %s\n" n_prop !max_acc_n !val_for_max
          else
            Printf.printf "PROMISED %d NONE\n" n_prop
        end else
          Printf.printf "PREPARE REJECTED %d\n" n_prop
      end else if op = "PROPOSE" then begin
        let n_prop = Scanf.scanf " %d" (fun x -> x) in
        let v = Scanf.scanf " %s" (fun x -> x) in
        let accepted_cnt = ref 0 in
        for i = 0 to n_acceptors - 1 do
          let acc = acceptors.(i) in
          if n_prop >= acc.promised_n then begin
            acc.promised_n <- n_prop;
            acc.accepted_n <- n_prop;
            acc.accepted_val <- v;
            incr accepted_cnt
          end
        done;
        if !accepted_cnt >= majority then
          Printf.printf "CHOSEN %d %s\n" n_prop v
        else
          Printf.printf "PROPOSE REJECTED %d\n" n_prop
      end
    done
  with End_of_file -> ()
