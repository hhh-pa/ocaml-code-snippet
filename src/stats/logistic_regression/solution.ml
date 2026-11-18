(* Logistic regression by IRLS (Think Stats 2e ch 11). *)

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

let gauss_solve mat vec beta =
  let k = Array.length mat in
  let singular = ref false in
  let s = ref 0 in
  while !s < k && not !singular do
    let piv = ref !s in
    for r = !s + 1 to k - 1 do
      if abs_float mat.(r).(!s) > abs_float mat.(!piv).(!s) then piv := r
    done;
    let tmp = mat.(!s) in
    mat.(!s) <- mat.(!piv);
    mat.(!piv) <- tmp;
    let tv = vec.(!s) in
    vec.(!s) <- vec.(!piv);
    vec.(!piv) <- tv;
    if mat.(!s).(!s) = 0.0 then singular := true
    else begin
      for r = !s + 1 to k - 1 do
        let factor = mat.(r).(!s) /. mat.(!s).(!s) in
        for c = !s to k - 1 do
          mat.(r).(c) <- mat.(r).(c) -. (factor *. mat.(!s).(c))
        done;
        vec.(r) <- vec.(r) -. (factor *. vec.(!s))
      done;
      incr s
    end
  done;
  if !singular then false
  else begin
    for s = k - 1 downto 0 do
      let tot = ref vec.(s) in
      for c = s + 1 to k - 1 do
        tot := !tot -. (mat.(s).(c) *. beta.(c))
      done;
      beta.(s) <- !tot /. mat.(s).(s)
    done;
    true
  end

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let arr = Array.of_list toks in
      let n = int_of_string arr.(0) in
      let p = int_of_string arr.(1) in
      let iters = int_of_string arr.(2) in
      let k = p + 1 in
      let rows =
        Array.init n (fun i ->
            Array.init k (fun j -> int_of_string arr.(3 + (i * k) + j)))
      in
      let beta = Array.make k 0.0 in
      let mat = Array.make_matrix k k 0.0 in
      let vec = Array.make k 0.0 in
      let cols = Array.make k 0.0 in
      let singular = ref false in
      let t = ref 0 in
      while !t < iters && not !singular do
        for i = 0 to k - 1 do
          for j = 0 to k - 1 do
            mat.(i).(j) <- 0.0
          done;
          vec.(i) <- 0.0
        done;
        for r = 0 to n - 1 do
          cols.(0) <- 1.0;
          for j = 1 to k - 1 do
            cols.(j) <- float_of_int rows.(r).(j - 1)
          done;
          let lin = ref 0.0 in
          for j = 0 to k - 1 do
            lin := !lin +. (cols.(j) *. beta.(j))
          done;
          let pr = 1.0 /. (1.0 +. exp (-. !lin)) in
          let w = pr *. (1.0 -. pr) in
          if w <> 0.0 then begin
            let z = !lin +. ((float_of_int rows.(r).(p) -. pr) /. w) in
            for i = 0 to k - 1 do
              for j = 0 to k - 1 do
                mat.(i).(j) <- mat.(i).(j) +. (w *. cols.(i) *. cols.(j))
              done;
              vec.(i) <- vec.(i) +. (w *. z *. cols.(i))
            done
          end
        done;
        if not (gauss_solve mat vec beta) then singular := true;
        incr t
      done;
      if !singular then print_string "SINGULAR"
      else
        let good = ref 0 in
        for r = 0 to n - 1 do
          let lin = ref beta.(0) in
          for j = 1 to k - 1 do
            lin := !lin +. (beta.(j) *. float_of_int rows.(r).(j - 1))
          done;
          let pr = 1.0 /. (1.0 +. exp (-. !lin)) in
          if pr >= 0.5 = (rows.(r).(p) = 1) then incr good
        done;
        let coeffs = Array.to_list (Array.map (Printf.sprintf "%.6f") beta) in
        Printf.printf "BETA %s\nACC %.6f" (String.concat " " coeffs)
          (float_of_int !good /. float_of_int n)
