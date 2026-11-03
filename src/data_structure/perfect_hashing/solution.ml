let p_prime = 1000003L

let () =
  let tokens =
    let buf = Buffer.create 4096 in
    (try while true do Buffer.add_channel buf stdin 4096 done with End_of_file -> ());
    let s = Buffer.contents buf in
    let len = String.length s in
    let res = ref [] in
    let i = ref 0 in
    while !i < len do
      while !i < len && (let c = s.[!i] in c = ' ' || c = '\t' || c = '\r' || c = '\n') do incr i done;
      if !i < len then begin
        let st = !i in
        while !i < len && (let c = s.[!i] in c <> ' ' && c <> '\t' && c <> '\r' && c <> '\n') do incr i done;
        res := String.sub s st (!i - st) :: !res
      end
    done;
    Array.of_list (List.rev !res)
  in
  if Array.length tokens >= 1 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let next_int64 () = Int64.of_string (next_str ()) in
    let n = next_int () in
    let keys = Array.init n (fun _ -> next_int64 ()) in
    let q = next_int () in
    let queries = Array.init q (fun _ -> next_int64 ()) in
    let m = if n > 0 then n else 1 in
    let m_i64 = Int64.of_int m in
    let best_a = ref 1L in
    let best_b = ref 0L in
    let best_sum = ref (-1L) in
    let cnt = Array.make m 0 in
    for a = 1 to 500 do
      let a_i64 = Int64.of_int a in
      for b = 0 to 500 do
        let b_i64 = Int64.of_int b in
        for j = 0 to m - 1 do
          cnt.(j) <- 0
        done;
        for i = 0 to n - 1 do
          let idx =
            Int64.to_int
              (Int64.rem
                (Int64.rem (Int64.add (Int64.mul a_i64 keys.(i)) b_i64) p_prime)
                m_i64)
          in
          cnt.(idx) <- cnt.(idx) + 1
        done;
        let s_acc = ref 0L in
        for j = 0 to m - 1 do
          let c = Int64.of_int cnt.(j) in
          s_acc := Int64.add !s_acc (Int64.mul c c)
        done;
        if !best_sum = -1L || !s_acc < !best_sum then begin
          best_sum := !s_acc;
          best_a := a_i64;
          best_b := b_i64
        end
      done
    done;
    let buckets = Array.init m (fun _ -> ref []) in
    for i = 0 to n - 1 do
      let idx =
        Int64.to_int
          (Int64.rem
            (Int64.rem (Int64.add (Int64.mul !best_a keys.(i)) !best_b) p_prime)
            m_i64)
      in
      buckets.(idx) := keys.(i) :: !(buckets.(idx))
    done;
    let buckets_arr = Array.init m (fun j -> Array.of_list (List.rev !(buckets.(j)))) in
    let a2 = Array.make m 1L in
    let b2 = Array.make m 0L in
    let m2 = Array.make m 1 in
    let slots = Array.init m (fun _ -> [||]) in
    let s_total = ref 0 in
    let seen = Array.make 250000 false in
    for j = 0 to m - 1 do
      let nj = Array.length buckets_arr.(j) in
      let mj = if nj > 1 then nj * nj else 1 in
      let mj_i64 = Int64.of_int mj in
      m2.(j) <- mj;
      s_total := !s_total + mj;
      let ba = ref 1L in
      let bb = ref 0L in
      let bc = ref (nj + 1) in
      let found_zero = ref false in
      let a = ref 1 in
      while !a <= 500 && not !found_zero do
        let a_i64 = Int64.of_int !a in
        let b = ref 0 in
        while !b <= 500 && not !found_zero do
          let b_i64 = Int64.of_int !b in
          for k = 0 to mj - 1 do
            seen.(k) <- false
          done;
          let col = ref 0 in
          for idx = 0 to nj - 1 do
            let k_val = buckets_arr.(j).(idx) in
            let slot =
              Int64.to_int
                (Int64.rem
                  (Int64.rem (Int64.add (Int64.mul a_i64 k_val) b_i64) p_prime)
                  mj_i64)
            in
            if seen.(slot) then incr col
            else seen.(slot) <- true
          done;
          if !col < !bc then begin
            bc := !col;
            ba := a_i64;
            bb := b_i64;
            if !bc = 0 then found_zero := true
          end;
          incr b
        done;
        incr a
      done;
      a2.(j) <- !ba;
      b2.(j) <- !bb;
      let bucket_slots = Array.init mj (fun _ -> ref []) in
      for idx = 0 to nj - 1 do
        let k_val = buckets_arr.(j).(idx) in
        let slot =
          Int64.to_int
            (Int64.rem
              (Int64.rem (Int64.add (Int64.mul !ba k_val) !bb) p_prime)
              mj_i64)
        in
        bucket_slots.(slot) := k_val :: !(bucket_slots.(slot))
      done;
      slots.(j) <- Array.init mj (fun s -> Array.of_list !(bucket_slots.(s)))
    done;
    Printf.printf "%d\n" !s_total;
    for i = 0 to q - 1 do
      let x = queries.(i) in
      let j =
        Int64.to_int
          (Int64.rem
            (Int64.rem (Int64.add (Int64.mul !best_a x) !best_b) p_prime)
            m_i64)
      in
      let s =
        Int64.to_int
          (Int64.rem
            (Int64.rem (Int64.add (Int64.mul a2.(j) x) b2.(j)) p_prime)
            (Int64.of_int m2.(j)))
      in
      let target_slot = slots.(j).(s) in
      let ok = ref false in
      for k = 0 to Array.length target_slot - 1 do
        if target_slot.(k) = x then ok := true
      done;
      if !ok then print_endline "YES"
      else print_endline "NO"
    done
  end
