let string_of_addr addr =
  match addr with
  | Unix.ADDR_UNIX a -> a
  | Unix.ADDR_INET (inet_addr, port) ->
    (Unix.string_of_inet_addr inet_addr) ^ ":" ^ (string_of_int port)
    
  
let () =
  let arglen = Array.length Sys.argv in
  if arglen <> 2 then
    print_endline "Usage: udp_ping_client <port>"
  else
    (* Bind UDP socket to listen to the response *)
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
    let my_addr = Unix.ADDR_INET ((Unix.inet_addr_of_string "0.0.0.0"), 9002) in
    let () = Unix.bind socket my_addr in
    (* Send "ping" message to the server *)
    let port = int_of_string Sys.argv.(1) in
    let server_addr = Unix.ADDR_INET ((Unix.inet_addr_of_string "0.0.0.0"), port) in
    let msg = Bytes.of_string "ping" in
    let _count = Unix.sendto socket msg 0 (Bytes.length msg) [] server_addr in
    (* Receive response from server and print it *)
    let bytes_recv = Bytes.create 512 in
    let count, addr  = Unix.recvfrom socket bytes_recv 0 512 [] in
    let response = String.of_bytes (Bytes.sub bytes_recv 0 count) in
    Format.printf "Received %s from %s" response (string_of_addr addr)
