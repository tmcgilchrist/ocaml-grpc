open Grpc_eio
open Health_check_proto.Health.Grpc.Health.V1
open Ocaml_protoc_plugin

type serving_status =
  | Unknown
  (* The service status is unknown. *)

  | Serving
  (* The service is currently up and serving requests. *)

  | NotServing
  (* The service is currently down and not serving requests. *)

let serving_status_to_string s =
  let open Health.Check.Response.ServingStatus in
  match s with
  | Unknown -> UNKNOWN
  | Serving -> SERVING
  | NotServing -> NOT_SERVING

let check buffer =
  let decode, encode = Service.make_service_functions Health.check in
  let _service_name =
    Reader.create buffer |> decode
    |> function
      | Ok v -> v
      | Error e -> failwith
                     (Printf.sprintf "Could not decode request: %s" (Result.show_error e))
  in
  (* TODO Lookup service health based on service_name. *)
  let reply = Health.Check.Response.(make ~status:ServingStatus.SERVING ()) in
  (Grpc.Status.(v OK), Some (encode reply |> Writer.contents))

let add_health routes check check_streaming =
  let service = Server.Service.(
      v ()
      |> add_rpc ~name:"Check" ~rpc:(Unary check)
      |> add_rpc ~name:"Watch" ~rpc:(Server_streaming check_streaming)
      |> handle_request) in
  Server.(routes |> add_service ~name:"grpc.health.v1" ~service)

