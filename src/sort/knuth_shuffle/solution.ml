(* https://leetcode.com/problems/shuffle-an-array/ *)
let () =
  try
    let a_const = 1103515245 and c_const = 12345 and m = 2147483648 in
    let seed = Scanf.scanf " %d" (fun x -> x) in
    let n = Scanf.scanf " %d" (fun x -> x) in
    let arr = Array.init (max n 0) (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    let n = Array.length arr in
    let state = ref (seed mod m) in
    for i = n - 1 downto 1 do
      state := ((!state * a_const) + c_const) mod m;
      let j = !state mod (i + 1) in
      let tmp = arr.(i) in
      arr.(i) <- arr.(j);
      arr.(j) <- tmp
    done;
    print_string "[";
    for i = 0 to n - 1 do
      if i > 0 then print_string ",";
      print_int arr.(i)
    done;
    print_string "]"
  with End_of_file -> ()
