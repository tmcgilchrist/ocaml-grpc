open Grpc_eio
open Health_check_proto.Health
open Ocaml_protoc_plugin

let add_health routes check check_streaming =
  let service = Server.Service.(
      v ()
      |> add_rpc ~name:"Check" ~rpc:(Unary check)
      |> add_rpc ~name:"Watch" ~rpc:(Server_streaming check_streaming)
      |> handle_request) in
  Server.(routes |> add_service ~name:"grpc.health.v1" ~service)

