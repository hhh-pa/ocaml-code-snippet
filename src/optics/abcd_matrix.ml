type mat = { a : float; b : float; c : float; d : float }

let mul x y =
  {
    a = x.a *. y.a +. x.b *. y.c;
    b = x.a *. y.b +. x.b *. y.d;
    c = x.c *. y.a +. x.d *. y.c;
    d = x.c *. y.b +. x.d *. y.d;
  }

let () =
  try
    let m = Scanf.scanf " %d" (fun x -> x) in
    let sys = ref { a = 1.0; b = 0.0; c = 0.0; d = 1.0 } in
    for _ = 1 to m do
      let kind = Scanf.scanf " %s" (fun s -> s) in
      let e =
        if kind = "free" then
          let dist = Scanf.scanf " %f" (fun x -> x) in
          { a = 1.0; b = dist; c = 0.0; d = 1.0 }
        else if kind = "lens" then
          let f = Scanf.scanf " %f" (fun x -> x) in
          { a = 1.0; b = 0.0; c = -.1.0 /. f; d = 1.0 }
        else if kind = "mirror" then
          let f = Scanf.scanf " %f" (fun x -> x) in
          { a = 1.0; b = 0.0; c = -.1.0 /. f; d = 1.0 }
        else if kind = "refract" then
          let n1 = Scanf.scanf " %f" (fun x -> x) in
          let n2 = Scanf.scanf " %f" (fun x -> x) in
          let r = Scanf.scanf " %f" (fun x -> x) in
          { a = 1.0; b = 0.0; c = (n1 -. n2) /. (n2 *. r); d = n1 /. n2 }
        else
          { a = 1.0; b = 0.0; c = 0.0; d = 1.0 }
      in
      sys := mul e !sys
    done;
    let y0 = Scanf.scanf " %f" (fun x -> x) in
    let theta0 = Scanf.scanf " %f" (fun x -> x) in
    let y1 = !sys.a *. y0 +. !sys.b *. theta0 in
    let th1 = !sys.c *. y0 +. !sys.d *. theta0 in
    Printf.printf "%.6f %.6f %.6f %.6f\n" !sys.a !sys.b !sys.c !sys.d;
    Printf.printf "%.6f %.6f\n" y1 th1
  with End_of_file -> ()
