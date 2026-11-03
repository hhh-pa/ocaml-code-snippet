let add a b =
  let n = Array.length a in
  let c = Array.make_matrix n n 0L in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      c.(i).(j) <- Int64.add a.(i).(j) b.(i).(j)
    done
  done;
  c

let sub a b =
  let n = Array.length a in
  let c = Array.make_matrix n n 0L in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      c.(i).(j) <- Int64.sub a.(i).(j) b.(i).(j)
    done
  done;
  c

let standard_mult a b =
  let n = Array.length a in
  let c = Array.make_matrix n n 0L in
  for i = 0 to n - 1 do
    let ai = a.(i) in
    let ci = c.(i) in
    for k = 0 to n - 1 do
      let aik = ai.(k) in
      let bk = b.(k) in
      for j = 0 to n - 1 do
        ci.(j) <- Int64.add ci.(j) (Int64.mul aik bk.(j))
      done
    done
  done;
  c

let rec strassen a b =
  let n = Array.length a in
  if n <= 16 then
    standard_mult a b
  else begin
    let k = n / 2 in
    let a11 = Array.make_matrix k k 0L in
    let a12 = Array.make_matrix k k 0L in
    let a21 = Array.make_matrix k k 0L in
    let a22 = Array.make_matrix k k 0L in
    let b11 = Array.make_matrix k k 0L in
    let b12 = Array.make_matrix k k 0L in
    let b21 = Array.make_matrix k k 0L in
    let b22 = Array.make_matrix k k 0L in

    for i = 0 to k - 1 do
      for j = 0 to k - 1 do
        a11.(i).(j) <- a.(i).(j);
        a12.(i).(j) <- a.(i).(j + k);
        a21.(i).(j) <- a.(i + k).(j);
        a22.(i).(j) <- a.(i + k).(j + k);

        b11.(i).(j) <- b.(i).(j);
        b12.(i).(j) <- b.(i).(j + k);
        b21.(i).(j) <- b.(i + k).(j);
        b22.(i).(j) <- b.(i + k).(j + k)
      done
    done;

    let m1 = strassen (add a11 a22) (add b11 b22) in
    let m2 = strassen (add a21 a22) b11 in
    let m3 = strassen a11 (sub b12 b22) in
    let m4 = strassen a22 (sub b21 b11) in
    let m5 = strassen (add a11 a12) b22 in
    let m6 = strassen (sub a21 a11) (add b11 b12) in
    let m7 = strassen (sub a12 a22) (add b21 b22) in

    let c11 = add (sub (add m1 m4) m5) m7 in
    let c12 = add m3 m5 in
    let c21 = add m2 m4 in
    let c22 = add (add (sub m1 m2) m3) m6 in

    let c = Array.make_matrix n n 0L in
    for i = 0 to k - 1 do
      for j = 0 to k - 1 do
        c.(i).(j) <- c11.(i).(j);
        c.(i).(j + k) <- c12.(i).(j);
        c.(i + k).(j) <- c21.(i).(j);
        c.(i + k).(j + k) <- c22.(i).(j)
      done
    done;
    c
  end

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = ref 1 in
    while !m < n do
      m := !m lsl 1
    done;
    let a = Array.make_matrix !m !m 0L in
    let b = Array.make_matrix !m !m 0L in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        a.(i).(j) <- Scanf.scanf " %Ld" (fun x -> x)
      done
    done;
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        b.(i).(j) <- Scanf.scanf " %Ld" (fun x -> x)
      done
    done;

    let c = strassen a b in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        if j > 0 then print_string " ";
        Printf.printf "%Ld" c.(i).(j)
      done;
      print_newline ()
    done
  with End_of_file -> ()
