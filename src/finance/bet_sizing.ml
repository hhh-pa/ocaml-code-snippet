let () =
  let tokens =
    let buf = Buffer.create 4096 in
    (try while true do Buffer.add_channel buf stdin 4096 done with End_of_file -> ());
    let s = Buffer.contents buf in
    let len = String.length s in
    let res = ref [] in
    let i = ref 0 in
    while !i < len do
      while !i < len && (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\r' || s.[!i] = '\n') do incr i done;
      if !i < len then begin
        let st = !i in
        while !i < len && not (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\r' || s.[!i] = '\n') do incr i done;
        res := String.sub s st (!i - st) :: !res
      end
    done;
    Array.of_list (List.rev !res)
  in
  if Array.length tokens >= 2 then begin
    let n = int_of_string tokens.(0) in
    let d = float_of_string tokens.(1) in
    let pr = Array.init n (fun i -> float_of_string tokens.(2 + i)) in
    let s = Array.init n (fun i -> 2.0 *. pr.(i) -. 1.0) in
    Printf.printf "SIZES:";
    Array.iter (fun x -> Printf.printf " %.6f" (x +. 0.0)) s;
    Printf.printf "\nACTIVE:";
    let pos = ref 0.0 in
    for i = 0 to n - 1 do
      pos := d *. !pos +. (1.0 -. d) *. s.(i);
      pos := max (-1.0) (min 1.0 !pos);
      Printf.printf " %.6f" (!pos +. 0.0)
    done;
    Printf.printf "\n"
  end
