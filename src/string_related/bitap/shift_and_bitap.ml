let shift_and_bitap_k_mismatch k pattern text =
  let m = String.length pattern in
  let n = String.length text in
  if m = 0 || m > n then []
  else begin
    let mask_all =
      if m = 64 then -1L
      else Int64.sub (Int64.shift_left 1L m) 1L
    in
    let char_mask = Array.make 26 0L in
    for i = 0 to m - 1 do
      let c = Char.code pattern.[i] - Char.code 'a' in
      char_mask.(c) <- Int64.logor char_mask.(c) (Int64.shift_left 1L i)
    done;

    let r = Array.make (k + 1) 0L in
    let accept_bit = Int64.shift_left 1L (m - 1) in
    let matches = ref [] in

    for j = 0 to n - 1 do
      let c = Char.code text.[j] - Char.code 'a' in
      let bc = char_mask.(c) in
      let prev_shifted = ref (Int64.logand (Int64.logor (Int64.shift_left r.(0) 1) 1L) mask_all) in
      r.(0) <- Int64.logand !prev_shifted bc;
      for d = 1 to k do
        let cur_shifted = Int64.logand (Int64.logor (Int64.shift_left r.(d) 1) 1L) mask_all in
        r.(d) <- Int64.logor (Int64.logand cur_shifted bc) !prev_shifted;
        prev_shifted := cur_shifted
      done;
      if j + 1 >= m && Int64.logand r.(k) accept_bit <> 0L then
        matches := (j + 1 - m) :: !matches
    done;
    List.rev !matches
  end

let solve_case () =
  let k = Scanf.scanf " %d" (fun x -> x) in
  let pattern = Scanf.scanf " %s" (fun x -> x) in
  let text = Scanf.scanf " %s" (fun x -> x) in
  let matches = shift_and_bitap_k_mismatch k pattern text in
  if matches = [] then
    Printf.printf "-1\n"
  else begin
    let first = ref true in
    List.iter (fun idx ->
      if not !first then print_string " ";
      print_int idx;
      first := false
    ) matches;
    print_newline ()
  end

let () =
  try
    let q = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to q - 1 do
      solve_case ()
    done
  with End_of_file -> ()
