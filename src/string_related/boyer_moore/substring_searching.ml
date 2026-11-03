let boyer_moore text pattern =
  let n = String.length text in
  let m = String.length pattern in
  if m = 0 then true
  else if m > n then false
  else begin
    let right = Array.make 256 (-1) in
    for j = 0 to m - 1 do
      right.(Char.code pattern.[j]) <- j
    done;
    let i = ref 0 in
    let found = ref false in
    while !i <= n - m && not !found do
      let skip = ref 0 in
      let j = ref (m - 1) in
      while !j >= 0 && !skip = 0 do
        if pattern.[!j] <> text.[!i + !j] then begin
          let s = !j - right.(Char.code text.[!i + !j]) in
          skip := if s < 1 then 1 else s
        end;
        decr j
      done;
      if !skip = 0 then found := true
      else i := !i + !skip
    done;
    !found
  end

let solve_case () =
  let text = Scanf.scanf " %s" (fun x -> x) in
  let pattern = Scanf.scanf " %s" (fun x -> x) in
  if boyer_moore text pattern then
    Printf.printf "YES\n"
  else
    Printf.printf "NO\n"

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to n - 1 do
      solve_case ()
    done
  with End_of_file -> ()
