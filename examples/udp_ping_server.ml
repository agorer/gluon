open Gluon

let ( let* ) = Result.bind
let log = Format.printf

let handle_error r =
  match r with
  | Ok x -> x
  | Error `Would_block -> Printf.sprintf "Would block\r\n%!" |> failwith
  | Error `No_info -> Printf.sprintf "No info\r\n%!" |> failwith
  | Error (`Exn exn) ->
      Printf.sprintf "Exn: %S\r\n%!" (Printexc.to_string exn) |> failwith
  | Error (`Unix_error err) ->
      Printf.sprintf "Unix error: %S\r\n%!" (Unix.error_message err) |> failwith
  | Error _ -> Printf.sprintf "other error" |> failwith

let run () =
  let port = 9001 in
  let addr = Net.Addr.udp Net.Addr.loopback port in
  log "listening on 0.0.0.0:%d\n%!" port;

  let* poll = Poll.make () in
  let server_token = Token.make 2112 in
  let* server = Net.Udp_listener.bind addr in

  let* () =
    Poll.register poll server_token Interest.readable
      (Net.Tcp_listener.to_source server)
  in

  let handle_event (_event : Event.t) =
    let data = Bytes.create 1024 in
    let* count, addr = Net.Udp_datagram.recv server data in
    let msg = Bytes.to_string (Bytes.sub data 0 count) in
    match msg with
    | "ping" ->
      log "Ping received from %a\n%!" Net.Addr.pp (addr :> Net.Addr.inet_addr);
      let _count = Net.Udp_datagram.send server (Bytes.of_string "pong") addr in
      Ok ()
    | _ -> log "Invalid message: %s\n%!" msg; Ok ()
  in

  let rec poll_loop () =
    let* events = Poll.poll ~max_events:100 poll in
    if List.length events > 0 then
      List.iter (fun e -> handle_event e |> handle_error) events;
    poll_loop ()
  in

  poll_loop ()

let () = handle_error (run ())
