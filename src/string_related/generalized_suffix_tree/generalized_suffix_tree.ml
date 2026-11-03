let read_all_tokens () =
  let content =
    let b = Buffer.create 4096 in
    let buf = Bytes.create 4096 in
    let rec loop () =
      let n = input stdin buf 0 4096 in
      if n > 0 then (
        Buffer.add_subbytes b buf 0 n;
        loop ()
      )
    in
    loop ();
    Buffer.contents b
  in
  let len = String.length content in
  let tokens = ref [] in
  let i = ref 0 in
  while !i < len do
    while !i < len && (let c = content.[!i] in c = ' ' || c = '\t' || c = '\r' || c = '\n') do
      incr i
    done;
    if !i < len then (
      let start = !i in
      while !i < len && (let c = content.[!i] in c <> ' ' && c <> '\t' && c <> '\r' && c <> '\n') do
        incr i
      done;
      tokens := String.sub content start (!i - start) :: !tokens
    )
  done;
  Array.of_list (List.rev !tokens)

module StringSet = Set.Make(String)

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 1 then (
    let k = int_of_string tokens.(0) in
    let strs = Array.init k (fun i -> tokens.(i + 1)) in
    if k = 0 then (
      Printf.printf "0\n\n"
    ) else if k = 1 then (
      Printf.printf "%d\n%s\n" (String.length strs.(0)) strs.(0)
    ) else (
      let hi = ref (String.length strs.(0)) in
      for i = 1 to k - 1 do
        let l = String.length strs.(i) in
        if l < !hi then hi := l
      done;

      let has_common l =
        if l = 0 then (true, "")
        else (
          let s0 = strs.(0) in
          let n0 = String.length s0 in
          let common = ref StringSet.empty in
          for i = 0 to n0 - l do
            common := StringSet.add (String.sub s0 i l) !common
          done;
          let ok = ref true in
          let t = ref 1 in
          while !t < k && !ok do
            let st = strs.(!t) in
            let nt = String.length st in
            let cur = ref StringSet.empty in
            for i = 0 to nt - l do
              let sub = String.sub st i l in
              if StringSet.mem sub !common then
                cur := StringSet.add sub !cur
            done;
            common := !cur;
            if StringSet.is_empty !common then ok := false;
            incr t
          done;
          if !ok && not (StringSet.is_empty !common) then
            (true, StringSet.min_elt !common)
          else
            (false, "")
        )
      in

      let low = ref 0 in
      let high = ref !hi in
      let best = ref 0 in
      let best_str = ref "" in
      while !low <= !high do
        let mid = (!low + !high) / 2 in
        let (ok, cur) = has_common mid in
        if ok then (
          best := mid;
          best_str := cur;
          low := mid + 1
        ) else (
          high := mid - 1
        )
      done;

      if !best > 0 then (
        let (_, cur) = has_common !best in
        best_str := cur
      );

      Printf.printf "%d\n%s\n" !best !best_str
    )
  )
