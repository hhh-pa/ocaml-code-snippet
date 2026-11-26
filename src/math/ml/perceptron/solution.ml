(* Perceptron (AIMA 19, Eq 19.8, exact rational weights). *)

let read_all () =
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_char buf (input_char stdin)
     done
   with End_of_file -> ());
  Buffer.contents buf

let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let split_ws s =
  let toks = ref [] in
  let cur = Buffer.create 16 in
  let flush () =
    if Buffer.length cur > 0 then begin
      toks := Buffer.contents cur :: !toks;
      Buffer.clear cur
    end
  in
  String.iter
    (fun c -> if is_space c then flush () else Buffer.add_char cur c)
    s;
  flush ();
  List.rev !toks

let lcg_mod = 1 lsl 31

let rec perc_gcd a b =
  let x = abs a and y = abs b in
  if y = 0 then x else perc_gcd y (x mod y)

let score num x =
  let s = ref num.(0) in
  for j = 0 to Array.length x - 1 do
    s := !s + (num.(j + 1) * x.(j))
  done;
  !s

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let t = Array.of_list (List.map int_of_string toks) in
      let d = t.(0) in
      let n = t.(1) in
      let u = t.(2) in
      let an = t.(3) in
      let ad = t.(4) in
      let rng = ref t.(5) in
      let feats =
        Array.init n (fun i ->
            Array.init d (fun j -> t.(6 + (i * (d + 1)) + j)))
      in
      let labels = Array.init n (fun i -> t.(6 + (i * (d + 1)) + d)) in
      let num = Array.make (d + 1) 0 in
      let mistakes = ref 0 in
      for _ = 1 to u do
        rng := ((1103515245 * !rng) + 12345) mod lcg_mod;
        let i = !rng mod n in
        let h = if score num feats.(i) >= 0 then 1 else 0 in
        if h <> labels.(i) then begin
          incr mistakes;
          let dl = an * (labels.(i) - h) in
          num.(0) <- num.(0) + dl;
          for j = 0 to d - 1 do
            num.(j + 1) <- num.(j + 1) + (dl * feats.(i).(j))
          done
        end
      done;
      let parts =
        Array.to_list
          (Array.map
             (fun v ->
               let g = perc_gcd v ad in
               Printf.sprintf "%d/%d" (v / g) (ad / g))
             num)
        |> String.concat " "
      in
      let correct = ref 0 in
      for i = 0 to n - 1 do
        if (if score num feats.(i) >= 0 then 1 else 0) = labels.(i) then
          incr correct
      done;
      Printf.printf "%s\nACC %d %d\nMISTAKES %d" parts !correct n !mistakes
