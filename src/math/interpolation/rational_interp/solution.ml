(* Rational interpolation (NR 3.4). *)

let fmt v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let () =
  let toks = String.split_on_char ' ' (String.concat " " (let rec rd acc =
    try rd (input_line stdin :: acc) with End_of_file -> List.rev acc in rd [])) in
  let toks = List.filter (fun s -> s <> "") toks in
  match toks with
  | [] -> ()
  | op :: rest ->
    let nums = List.map float_of_string rest in
    let arr = Array.of_list nums in
    let n = int_of_float arr.(0) in
    let xa = Array.init n (fun i -> arr.(1 + i)) in
    let ya = Array.init n (fun i -> arr.(1 + n + i)) in
    let restv = Array.sub arr (1 + 2 * n) (Array.length arr - 1 - 2 * n) in
    if op = "RATINT" then begin
      let tiny = 1.0e-99 in
      let q = int_of_float restv.(0) in
      for qq = 0 to q - 1 do
        let x = restv.(1 + qq) in
        let c = Array.copy ya in
        let d = Array.init n (fun i -> ya.(i) +. tiny) in
        let hh = ref (abs_float (x -. xa.(0))) in
        let ns = ref 0 and exact = ref (-1) in
        for i = 0 to n - 1 do
          let h = abs_float (x -. xa.(i)) in
          if h = 0.0 then exact := i;
          if h < !hh && !exact < 0 then begin ns := i; hh := h end
        done;
        if !exact >= 0 then Printf.printf "%s %s\n" (fmt ya.(!exact)) (fmt 0.0)
        else begin
          let y = ref ya.(!ns) in
          decr ns;
          let dy = ref 0.0 in
          for m = 1 to n - 1 do
            for i = 0 to n - m - 1 do
              let w = c.(i + 1) -. d.(i) in
              let h = xa.(i + m) -. x in
              let t = (xa.(i) -. x) *. d.(i) /. h in
              let dd = ref (t -. c.(i + 1)) in
              dd := w /. !dd;
              d.(i) <- c.(i + 1) *. !dd;
              c.(i) <- t *. !dd
            done;
            if 2 * (!ns + 1) < (n - m) then dy := c.(!ns + 1)
            else begin dy := d.(!ns); decr ns end;
            y := !y +. !dy
          done;
          Printf.printf "%s %s\n" (fmt !y) (fmt !dy)
        end
      done
    end else begin
      let ddord = int_of_float restv.(0) in
      let q = int_of_float restv.(1) in
      let w = Array.make n 0.0 in
      for k = 0 to n - 1 do
        let imin = max (k - ddord) 0 in
        let imax = if k >= n - ddord then n - ddord - 1 else k in
        let temp = ref (if imin land 1 = 1 then -1.0 else 1.0) in
        let sum = ref 0.0 in
        for i = imin to imax do
          let jmax = min (i + ddord) (n - 1) in
          let term = ref 1.0 in
          for j = i to jmax do
            if j <> k then term := !term *. (xa.(k) -. xa.(j))
          done;
          term := !temp /. !term;
          temp := -. !temp;
          sum := !sum +. !term
        done;
        w.(k) <- !sum
      done;
      for qq = 0 to q - 1 do
        let x = restv.(2 + qq) in
        let num = ref 0.0 and den = ref 0.0 and exact = ref (-1) in
        for i = 0 to n - 1 do
          let h = x -. xa.(i) in
          if h = 0.0 then exact := i
          else begin
            let t = w.(i) /. h in
            num := !num +. t *. ya.(i);
            den := !den +. t
          end
        done;
        if !exact >= 0 then print_endline (fmt ya.(!exact))
        else print_endline (fmt (!num /. !den))
      done
    end
