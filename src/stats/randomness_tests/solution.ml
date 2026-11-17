(* RNG statistical battery: monobit/poker/runs/long-runs/byte cores. *)

let read_all () =
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_char buf (input_char stdin)
     done
   with End_of_file -> ());
  Buffer.contents buf

let () =
  let bits = String.trim (read_all ()) in
  let n = String.length bits in
  if n > 0 then begin
    let ones = ref 0 in
    String.iter (fun c -> if c = '1' then incr ones) bits;
    let nibs = Array.make 16 0 in
    let i = ref 0 in
    while !i < n do
      let v = ref 0 in
      for j = 0 to 3 do
        v := (!v * 2) + if bits.[!i + j] = '1' then 1 else 0
      done;
      nibs.(!v) <- nibs.(!v) + 1;
      i := !i + 4
    done;
    let poker = Array.fold_left (fun a v -> a + (v * v)) 0 nibs in
    let runs = ref 1
    and longrun = ref 0
    and curlen = ref 1
    and cur = ref bits.[0] in
    for k = 1 to n - 1 do
      if bits.[k] = !cur then incr curlen
      else begin
        if !cur = '1' then longrun := max !longrun !curlen;
        incr runs;
        cur := bits.[k];
        curlen := 1
      end
    done;
    if !cur = '1' then longrun := max !longrun !curlen;
    let bc = Array.make 256 0 in
    let j = ref 0 in
    while !j < n do
      let v = ref 0 in
      for k = 0 to 7 do
        v := (!v * 2) + if bits.[!j + k] = '1' then 1 else 0
      done;
      bc.(!v) <- bc.(!v) + 1;
      j := !j + 8
    done;
    let bytesq = Array.fold_left (fun a v -> a + (v * v)) 0 bc in
    Printf.printf "%d %d %d %d %d %d" n !ones poker !runs !longrun bytesq
  end
