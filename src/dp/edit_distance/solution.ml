let edit_distance a b =
  let m = String.length a in
  let n = String.length b in
  let dp0 = Array.init (m + 1) (fun i -> i) in
  let dp1 = Array.make (m + 1) 0 in
  let cur = ref dp1 in
  let prev = ref dp0 in
  for i = 1 to n do
    !cur.(0) <- i;
    let bi = b.[i - 1] in
    for j = 1 to m do
      if a.[j - 1] = bi then
        !cur.(j) <- !prev.(j - 1)
      else
        !cur.(j) <- 1 + min !prev.(j) (min !cur.(j - 1) !prev.(j - 1))
    done;
    let tmp = !prev in
    prev := !cur;
    cur := tmp
  done;
  !prev.(m)

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    for _ = 1 to n do
      let a = Scanf.scanf " %s" (fun s -> s) in
      let b = Scanf.scanf " %s" (fun s -> s) in
      Printf.printf "%d\n" (edit_distance a b)
    done
  with End_of_file -> ()
