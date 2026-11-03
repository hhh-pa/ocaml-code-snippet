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

type rule = {
  lhs : int;
  rhs1 : int;
  rhs2 : int;
  term : int;
}

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 2 then (
    let num_rules = int_of_string tokens.(0) in
    let start_symbol = Char.code tokens.(1).[0] in
    let idx = ref 2 in
    let rules = ref [] in
    for _ = 1 to num_rules do
      if !idx + 2 < Array.length tokens then (
        let lhs = Char.code tokens.(!idx).[0] in
        let _arrow = tokens.(!idx + 1) in
        let rhs = tokens.(!idx + 2) in
        idx := !idx + 3;
        if String.length rhs = 1 then
          rules := { lhs; rhs1 = 0; rhs2 = 0; term = Char.code rhs.[0] } :: !rules
        else if String.length rhs = 2 then
          rules := { lhs; rhs1 = Char.code rhs.[0]; rhs2 = Char.code rhs.[1]; term = 0 } :: !rules
      )
    done;
    let rules = Array.of_list (List.rev !rules) in
    if !idx < Array.length tokens then (
      let num_queries = int_of_string tokens.(!idx) in
      incr idx;
      let m_mod = 1000000007L in
      for _ = 1 to num_queries do
        if !idx < Array.length tokens then (
          let w = tokens.(!idx) in
          incr idx;
          let n = String.length w in
          if n = 0 then
            Printf.printf "NO 0\n"
          else (
            let dp = Array.init (n + 1) (fun _ ->
              Array.init n (fun _ ->
                Array.make 256 0L
              )
            ) in
            for i = 0 to n - 1 do
              let ch = Char.code w.[i] in
              Array.iter (fun r ->
                if r.term = ch then
                  dp.(1).(i).(r.lhs) <- Int64.add dp.(1).(i).(r.lhs) 1L
              ) rules
            done;
            for len = 2 to n do
              for i = 0 to n - len do
                for k = 1 to len - 1 do
                  let left_len = k in
                  let right_len = len - k in
                  let left_start = i in
                  let right_start = i + k in
                  Array.iter (fun r ->
                    if r.rhs1 <> 0 then (
                      let left_ways = dp.(left_len).(left_start).(r.rhs1) in
                      if left_ways <> 0L then (
                        let right_ways = dp.(right_len).(right_start).(r.rhs2) in
                        if right_ways <> 0L then (
                          let add = Int64.rem (Int64.mul (Int64.rem left_ways m_mod) (Int64.rem right_ways m_mod)) m_mod in
                          dp.(len).(i).(r.lhs) <- Int64.rem (Int64.add dp.(len).(i).(r.lhs) add) m_mod
                        )
                      )
                    )
                  ) rules
                done
              done
            done;
            let ways = dp.(n).(0).(start_symbol) in
            if ways > 0L then
              Printf.printf "YES %Ld\n" ways
            else
              Printf.printf "NO 0\n"
          )
        )
      done
    )
  )
