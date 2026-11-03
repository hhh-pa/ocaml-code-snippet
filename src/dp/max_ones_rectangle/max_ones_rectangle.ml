let largest_histogram_area heights =
  let w = Array.length heights in
  let hh = Array.make (w + 1) 0 in
  Array.blit heights 0 hh 0 w;
  let st = Stack.create () in
  let best = ref 0 in
  for i = 0 to w do
    let h = hh.(i) in
    while not (Stack.is_empty st) && hh.(Stack.top st) > h do
      let top_idx = Stack.pop st in
      let height = hh.(top_idx) in
      let width = if Stack.is_empty st then i else i - Stack.top st - 1 in
      let area = height * width in
      if area > !best then best := area
    done;
    Stack.push i st
  done;
  !best

let () =
  try
    let h = Scanf.scanf " %d" (fun x -> x) in
    let w = Scanf.scanf " %d" (fun x -> x) in
    let g = Array.make_matrix h w 0 in
    for r = 0 to h - 1 do
      for c = 0 to w - 1 do
        g.(r).(c) <- Scanf.scanf " %d" (fun x -> x)
      done
    done;
    let dp = Array.make_matrix h w 0 in
    let ms = ref 0 in
    for r = 0 to h - 1 do
      for c = 0 to w - 1 do
        if g.(r).(c) <> 0 then begin
          if r = 0 || c = 0 then
            dp.(r).(c) <- 1
          else
            dp.(r).(c) <- 1 + min dp.(r - 1).(c) (min dp.(r).(c - 1) dp.(r - 1).(c - 1));
          if dp.(r).(c) > !ms then ms := dp.(r).(c)
        end
      done
    done;
    let heights = Array.make w 0 in
    let mr = ref 0 in
    for r = 0 to h - 1 do
      for c = 0 to w - 1 do
        heights.(c) <- (if g.(r).(c) <> 0 then heights.(c) + 1 else 0)
      done;
      let area = largest_histogram_area heights in
      if area > !mr then mr := area
    done;
    Printf.printf "%d\n" (!ms * !ms);
    Printf.printf "%d\n" !mr
  with End_of_file -> ()
