type process = {
  id: int;
  mutable balance: int;
  mutable recorded: bool;
  mutable recorded_balance: int;
}

type message = {
  from_p: int;
  to_p: int;
  text: string;
}

let () =
  try
    let n_procs = Scanf.scanf " %d" (fun x -> x) in
    let n_events = Scanf.scanf " %d" (fun x -> x) in
    let procs = Array.init n_procs (fun i ->
      let bal = Scanf.scanf " %d" (fun x -> x) in
      { id = i; balance = bal; recorded = false; recorded_balance = 0 }
    ) in
    let channels = Array.init n_procs (fun _ -> Array.init n_procs (fun _ -> Queue.create ())) in
    let recorded_channels = Array.init n_procs (fun _ -> Array.init n_procs (fun _ -> ref [])) in
    let is_recording = Array.init n_procs (fun _ -> Array.make n_procs false) in
    for _ = 0 to n_events - 1 do
      let event_type = Scanf.scanf " %s" (fun x -> x) in
      if event_type = "TRANSFER" then begin
        let u = Scanf.scanf " %d" (fun x -> x) in
        let v = Scanf.scanf " %d" (fun x -> x) in
        let amt = Scanf.scanf " %d" (fun x -> x) in
        procs.(u).balance <- procs.(u).balance - amt;
        Queue.push { from_p = u; to_p = v; text = string_of_int amt } channels.(u).(v)
      end else if event_type = "RECEIVE" then begin
        let u = Scanf.scanf " %d" (fun x -> x) in
        let v = Scanf.scanf " %d" (fun x -> x) in
        if not (Queue.is_empty channels.(u).(v)) then begin
          let msg = Queue.pop channels.(u).(v) in
          if msg.text = "MARKER" then begin
            if not procs.(v).recorded then begin
              procs.(v).recorded <- true;
              procs.(v).recorded_balance <- procs.(v).balance;
              for k = 0 to n_procs - 1 do
                if k <> v && k <> u then
                  is_recording.(k).(v) <- true
              done;
              for k = 0 to n_procs - 1 do
                if k <> v then
                  Queue.push { from_p = v; to_p = k; text = "MARKER" } channels.(v).(k)
              done
            end else
              is_recording.(u).(v) <- false
          end else begin
            let amt = int_of_string msg.text in
            procs.(v).balance <- procs.(v).balance + amt;
            if is_recording.(u).(v) then
              recorded_channels.(u).(v) := msg.text :: !(recorded_channels.(u).(v))
          end
        end
      end else if event_type = "SNAPSHOT_INIT" then begin
        let init_p = Scanf.scanf " %d" (fun x -> x) in
        if not procs.(init_p).recorded then begin
          procs.(init_p).recorded <- true;
          procs.(init_p).recorded_balance <- procs.(init_p).balance;
          for k = 0 to n_procs - 1 do
            if k <> init_p then begin
              is_recording.(k).(init_p) <- true;
              Queue.push { from_p = init_p; to_p = k; text = "MARKER" } channels.(init_p).(k)
            end
          done
        end
      end
    done;
    let total_snapshot_money = ref 0 in
    for i = 0 to n_procs - 1 do
      Printf.printf "P%d: %d\n" i procs.(i).recorded_balance;
      total_snapshot_money := !total_snapshot_money + procs.(i).recorded_balance
    done;
    for u = 0 to n_procs - 1 do
      for v = 0 to n_procs - 1 do
        let msgs = List.rev !(recorded_channels.(u).(v)) in
        List.iter (fun msg_text ->
          Printf.printf "CHANNEL %d->%d: %s\n" u v msg_text;
          total_snapshot_money := !total_snapshot_money + int_of_string msg_text
        ) msgs
      done
    done;
    Printf.printf "TOTAL: %d\n" !total_snapshot_money
  with End_of_file -> ()
