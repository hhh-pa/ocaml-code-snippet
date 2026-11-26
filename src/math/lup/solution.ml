(* LUP decomposition + solve with partial pivoting (CLRS 28). *)

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

let lup_decompose a perm =
  let n = Array.length a in
  let singular = ref false in
  for k = 0 to n - 1 do
    if not !singular then begin
      let piv = ref k in
      for i = k + 1 to n - 1 do
        if abs_float a.(i).(k) > abs_float a.(!piv).(k) then piv := i
      done;
      if a.(!piv).(k) = 0.0 then singular := true
      else begin
        if !piv <> k then begin
          let t = a.(k) in
          a.(k) <- a.(!piv);
          a.(!piv) <- t;
          let p = perm.(k) in
          perm.(k) <- perm.(!piv);
          perm.(!piv) <- p
        end;
        for i = k + 1 to n - 1 do
          a.(i).(k) <- a.(i).(k) /. a.(k).(k);
          for j = k + 1 to n - 1 do
            a.(i).(j) <- a.(i).(j) -. (a.(i).(k) *. a.(k).(j))
          done
        done
      end
    end
  done;
  not !singular

let lup_solve a perm b =
  let n = Array.length a in
  let y = Array.make n 0.0 in
  for i = 0 to n - 1 do
    let s = ref b.(perm.(i)) in
    for j = 0 to i - 1 do
      s := !s -. (a.(i).(j) *. y.(j))
    done;
    y.(i) <- !s
  done;
  let x = Array.make n 0.0 in
  for i = n - 1 downto 0 do
    let s = ref y.(i) in
    for j = i + 1 to n - 1 do
      s := !s -. (a.(i).(j) *. x.(j))
    done;
    x.(i) <- !s /. a.(i).(i)
  done;
  x

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let nums = Array.of_list (List.map float_of_string toks) in
      let n = int_of_float nums.(0) in
      let a = Array.make_matrix n n 0.0 in
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          a.(i).(j) <- nums.(1 + (i * n) + j)
        done
      done;
      let b = Array.init n (fun i -> nums.(1 + (n * n) + i)) in
      let perm = Array.init n (fun i -> i) in
      if not (lup_decompose a perm) then print_string "SINGULAR"
      else
        let x = lup_solve a perm b in
        let parts =
          Array.to_list
            (Array.map
               (fun v ->
                 Printf.sprintf "%.6f" (if abs_float v < 5e-7 then 0.0 else v))
               x)
        in
        print_string (String.concat " " parts)
