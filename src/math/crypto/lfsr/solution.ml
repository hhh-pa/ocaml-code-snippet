(* Linear feedback shift registers (NR 7.5). *)

let nn = 32
let factors = [| 3; 5; 17; 257; 65537 |]

let matmul_into dst x y =
  let tmp = Array.make (nn * nn) 0 in
  for i = 0 to nn - 1 do
    for j = 0 to nn - 1 do
      let s = ref 0 in
      for k = 0 to nn - 1 do
        s := !s + x.(i * nn + k) * y.(k * nn + j)
      done;
      tmp.(i * nn + j) <- !s land 1
    done
  done;
  Array.blit tmp 0 dst 0 (nn * nn)

let primpolytest n =
  let t = Array.make (nn * nn) 0
  and a = Array.make (nn * nn) 0
  and p = Array.make (nn * nn) 0 in
  let tnm1 = (1 lsl 32) - 1 in
  if n > tnm1 lsr 1 then 0
  else (
    for i = 1 to nn - 1 do
      t.(i * nn + (i - 1)) <- 1
    done;
    let j = ref 0 and m = ref n in
    while !m <> 0 do
      if !m land 1 <> 0 then t.(!j) <- 1;
      m := !m lsr 1;
      j := !j + 1
    done;
    t.(nn - 1) <- 1;
    let res = ref 1 and stop = ref false and rnd = ref 0 in
    while !rnd < 6 && not !stop do
      let pw = if !rnd = 0 then tnm1 else tnm1 / factors.(!rnd - 1) in
      Array.blit t 0 a 0 (nn * nn);
      for k = 0 to nn * nn - 1 do
        p.(k) <- 0
      done;
      for k = 0 to nn - 1 do
        p.(k * nn + k) <- 1
      done;
      let e = ref pw in
      let cont = ref true in
      while !cont do
        (if !e land 1 <> 0 then matmul_into p p a);
        e := !e lsr 1;
        if !e = 0 then cont := false else matmul_into a a a
      done;
      let ident = ref 1 in
      for i = 0 to nn - 1 do
        for k = 0 to nn - 1 do
          if i = k then (if p.(i * nn + k) <> 1 then ident := 0)
          else if p.(i * nn + k) <> 0 then ident := 0
        done
      done;
      if !rnd = 0 then (if !ident <> 1 then (res := 0; stop := true))
      else if !ident = 1 then (res := 0; stop := true);
      rnd := !rnd + 1
    done;
    !res)

let () =
  let toks =
    String.split_on_char ' '
      (String.concat " "
         (let rec rd acc =
            try rd (input_line stdin :: acc) with End_of_file -> List.rev acc
          in
          rd []))
  in
  let toks = Array.of_list (List.filter (fun s -> s <> "") toks) in
  if Array.length toks = 0 then exit 0;
  let op = toks.(0) in
  if op = "STEP" then (
    let maskp = int_of_string toks.(1)
    and maskn = int_of_string toks.(2)
    and a = ref (int_of_string toks.(3)) in
    let n = int_of_string toks.(4) in
    let out = ref [] in
    for _i = 1 to n do
      (if !a land maskn <> 0 then a := ((!a lxor maskp) lsl 1) lor 1
       else a := !a lsl 1);
      a := !a land 0xFFFFFFFF;
      out := string_of_int !a :: !out
    done;
    print_endline (String.concat " " (List.rev !out)))
  else if op = "ISPRIMITIVE" then print_endline (string_of_int (primpolytest (int_of_string toks.(1))))
