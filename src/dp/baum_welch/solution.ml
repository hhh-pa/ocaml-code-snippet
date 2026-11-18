(* Baum-Welch HMM training (AIMA 21.3.3, scaled forward-backward). *)

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

let bw_forward pi a b obs =
  let s = Array.length pi in
  let t = Array.length obs in
  let al = Array.make_matrix t s 0.0 in
  let c = Array.make t 0.0 in
  for i = 0 to s - 1 do
    al.(0).(i) <- pi.(i) *. b.(i).(obs.(0))
  done;
  for i = 0 to s - 1 do
    c.(0) <- c.(0) +. al.(0).(i)
  done;
  for i = 0 to s - 1 do
    al.(0).(i) <- al.(0).(i) /. c.(0)
  done;
  for tt = 1 to t - 1 do
    for j = 0 to s - 1 do
      let tot = ref 0.0 in
      for i = 0 to s - 1 do
        tot := !tot +. (al.(tt - 1).(i) *. a.(i).(j))
      done;
      al.(tt).(j) <- b.(j).(obs.(tt)) *. !tot
    done;
    for j = 0 to s - 1 do
      c.(tt) <- c.(tt) +. al.(tt).(j)
    done;
    for j = 0 to s - 1 do
      al.(tt).(j) <- al.(tt).(j) /. c.(tt)
    done
  done;
  (al, c)

let bw_backward a b obs c =
  let s = Array.length a in
  let t = Array.length obs in
  let be = Array.make_matrix t s 0.0 in
  for i = 0 to s - 1 do
    be.(t - 1).(i) <- 1.0
  done;
  for tt = t - 2 downto 0 do
    for i = 0 to s - 1 do
      let tot = ref 0.0 in
      for j = 0 to s - 1 do
        tot := !tot +. (a.(i).(j) *. b.(j).(obs.(tt + 1)) *. be.(tt + 1).(j))
      done;
      be.(tt).(i) <- !tot /. c.(tt + 1)
    done
  done;
  be

let bw_gammas al be =
  let t = Array.length al in
  let s = Array.length al.(0) in
  let gam = Array.make_matrix t s 0.0 in
  for tt = 0 to t - 1 do
    let tot = ref 0.0 in
    for i = 0 to s - 1 do
      tot := !tot +. (al.(tt).(i) *. be.(tt).(i))
    done;
    for i = 0 to s - 1 do
      gam.(tt).(i) <- al.(tt).(i) *. be.(tt).(i) /. !tot
    done
  done;
  gam

let xi_prod al be a b obs tt i j =
  al.(tt).(i) *. a.(i).(j) *. b.(j).(obs.(tt + 1)) *. be.(tt + 1).(j)

let bw_xis al be a b obs =
  let s = Array.length a in
  let t = Array.length obs in
  let xi = Array.init (max 0 (t - 1)) (fun _ -> Array.make_matrix s s 0.0) in
  for tt = 0 to t - 2 do
    let tot = ref 0.0 in
    for i = 0 to s - 1 do
      for j = 0 to s - 1 do
        tot := !tot +. xi_prod al be a b obs tt i j
      done
    done;
    for i = 0 to s - 1 do
      for j = 0 to s - 1 do
        xi.(tt).(i).(j) <- xi_prod al be a b obs tt i j /. !tot
      done
    done
  done;
  xi

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let t = Array.of_list toks in
      let pos = ref 0 in
      let next () =
        let v = t.(!pos) in
        incr pos;
        v
      in
      let s = int_of_string (next ()) in
      let o = int_of_string (next ()) in
      let nl = int_of_string (next ()) in
      let iters = int_of_string (next ()) in
      let pi = Array.init s (fun _ -> float_of_string (next ())) in
      let a =
        Array.init s (fun _ ->
            Array.init s (fun _ -> float_of_string (next ())))
      in
      let b =
        Array.init s (fun _ ->
            Array.init o (fun _ -> float_of_string (next ())))
      in
      let seqs =
        List.init nl (fun _ ->
            let len = int_of_string (next ()) in
            Array.init len (fun _ -> int_of_string (next ())))
      in
      for _ = 1 to iters do
        let pi_num = Array.make s 0.0 in
        let a_num = Array.make_matrix s s 0.0 in
        let a_den = Array.make s 0.0 in
        let b_num = Array.make_matrix s o 0.0 in
        let b_den = Array.make s 0.0 in
        List.iter
          (fun obs ->
            let tt_len = Array.length obs in
            let al, c = bw_forward pi a b obs in
            let be = bw_backward a b obs c in
            let gam = bw_gammas al be in
            let xi = bw_xis al be a b obs in
            for i = 0 to s - 1 do
              pi_num.(i) <- pi_num.(i) +. gam.(0).(i)
            done;
            for tt = 0 to tt_len - 2 do
              for i = 0 to s - 1 do
                a_den.(i) <- a_den.(i) +. gam.(tt).(i);
                for j = 0 to s - 1 do
                  a_num.(i).(j) <- a_num.(i).(j) +. xi.(tt).(i).(j)
                done
              done
            done;
            for tt = 0 to tt_len - 1 do
              for j = 0 to s - 1 do
                b_den.(j) <- b_den.(j) +. gam.(tt).(j);
                b_num.(j).(obs.(tt)) <- b_num.(j).(obs.(tt)) +. gam.(tt).(j)
              done
            done)
          seqs;
        for i = 0 to s - 1 do
          pi.(i) <- pi_num.(i) /. float_of_int nl;
          if a_den.(i) > 0.0 then
            for j = 0 to s - 1 do
              a.(i).(j) <- a_num.(i).(j) /. a_den.(i)
            done;
          if b_den.(i) > 0.0 then
            for k = 0 to o - 1 do
              b.(i).(k) <- b_num.(i).(k) /. b_den.(i)
            done
        done
      done;
      let out = Buffer.create 256 in
      for i = 0 to s - 1 do
        if i > 0 then Buffer.add_char out ' ';
        Buffer.add_string out (Printf.sprintf "%.6f" pi.(i))
      done;
      Buffer.add_char out '\n';
      for i = 0 to s - 1 do
        for j = 0 to s - 1 do
          if j > 0 then Buffer.add_char out ' ';
          Buffer.add_string out (Printf.sprintf "%.6f" a.(i).(j))
        done;
        Buffer.add_char out '\n'
      done;
      for i = 0 to s - 1 do
        for k = 0 to o - 1 do
          if k > 0 then Buffer.add_char out ' ';
          Buffer.add_string out (Printf.sprintf "%.6f" b.(i).(k))
        done;
        if i < s - 1 then Buffer.add_char out '\n'
      done;
      print_string (Buffer.contents out)
