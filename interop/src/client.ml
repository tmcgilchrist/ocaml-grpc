open Grpc_eio

let client ~sw host port network =
  let inet, port =
    Eio_unix.run_in_systhread (fun () ->
        Unix.getaddrinfo host port [ Unix.(AI_FAMILY PF_INET) ])
    |> List.filter_map (fun (addr : Unix.addr_info) ->
           match addr.ai_addr with
           | Unix.ADDR_UNIX _ -> None
           | ADDR_INET (addr, port) -> Some (addr, port))
    |> List.hd
  in
  let addr = `Tcp (Eio_unix.Net.Ipaddr.of_unix inet, port) in
  let socket = Eio.Net.connect ~sw network addr in
  H2_eio.Client.create_connection ~sw ~error_handler:ignore
    (socket :> Eio.Flow.two_way)

let call_empty_unary connection =
  let open Ocaml_protoc_plugin in
  let open Interop_proto.Test.Grpc.Testing in
  Eio.traceln "EIO call_empty_unary";
  let encode, decode = Service.make_client_functions TestService.emptyCall in
  let service = "grpc.testing.TestService" in
  let rpc = "EmptyCall" in
  let do_request = H2_eio.Client.request connection ~error_handler:ignore in
  let request = TestService.EmptyCall.Request.make () in
  let handler = Client.Rpc.unary (encode request |> Writer.contents)
                  ~f:(fun response ->
                    Eio.traceln "response";
                    match response with
                                      | Some response ->
                                         (Reader.create response
                                          |> decode
                                          |> function
                                            | Ok () -> ()
                                            | Error e ->
                                               failwith
                                                 (Printf.sprintf "Could not decode request: %s"
                                                    (Result.show_error e)))
                                      | None -> failwith
                                                  (Printf.sprintf "Expected request got None")) in
  let response = Client.call ~service ~rpc ~do_request ~handler () in
  match response with
  | Ok ((), status) when Grpc.Status.code status == OK -> Eio.traceln "no error"
  | Ok ((), _status)  -> Eio.traceln "an error occurred"
  | Error _ -> Eio.traceln "an error occurred"

let main env server_host _server_host_override server_port test_case _use_tls _use_test_ca _default_service_account _oauth_scope _service_account_key_file _service_config_json _additional_metadata =
  let network = Eio.Stdenv.net env in
  let run sw =
    let connection = client ~sw server_host server_port network in

    match test_case with
    | "empty_unary" -> call_empty_unary connection
    | _ -> failwith "Unimplemented"
  in
  Eio.Switch.run run

(* Command-line parsing *)

open Cmdliner

let server_host =
  Arg.value
  @@ Arg.opt Arg.string "localhost"
  @@ Arg.info ~doc:"The server host to connect to. For example, 'localhost' or '127.0.0.1'" ~docv:"HOSTNAME" [ "server_host" ]

let server_host_override =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"The server host to claim to be connecting to, for use in TLS and HTTP/2 :authority header. If unspecified, the value of --server_host will be used" ~docv:"HOSTNAME" [ "server_host_override" ]

let server_port =
  Arg.value
  @@ Arg.opt Arg.string "10000"
  @@ Arg.info ~doc:"The server port to connect to. For example, '8080'" ~docv:"PORT" [ "server_port" ]

let test_case =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"The name of the test case to execute. For example, 'empty_unary'" ~docv:"TESTCASE" [ "test_case" ]

let use_tls =
  Arg.value
  @@ Arg.opt Arg.(some bool) None
  @@ Arg.info ~doc:"Whether to use a plaintext or encrypted connection"  [ "use_tls" ]

let use_test_ca =
  Arg.value
  @@ Arg.opt Arg.(some bool) None
  @@ Arg.info ~doc:"Whether to replace platform root CAs with ca.pem as the CA root" [ "use_test_ca" ]

let default_service_account =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"Email of the GCE default service account." ~docv:"ACCOUNT_EMAIL" [ "default_service_account" ]

let oauth_scope =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"OAuth scope. For example, 'https://www.googleapis.com/auth/xapi.zoo'" ~docv:"SCOPE" [ "oauth_scope" ]

let service_account_key_file =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"The path to the service account JSON key file generated from GCE developer console." [ "service_account_key_file" ]

let service_config_json =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"Disables service config lookups and sets the provided string as the default service config." ~docv:"SERVICE_CONFIG_JSON" [ "service_config_json" ]

let additional_metadata =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"Additional metadata to send in each request, as a semicolon-separated list of key:value pairs." ~docv:"ADDITIONAL_METADATA" [ "additional_metadata" ]

let cmd env =
  let doc = "Client for interop tests" in
  let info = Cmd.info "client-interop" ~doc in
  Cmd.v info
    Term.(const main $ (const env) $ server_host $ server_host_override $ server_port $ test_case $ use_tls $ use_test_ca $ default_service_account $ oauth_scope $ service_account_key_file $ service_config_json $ additional_metadata)

let () =
  exit @@
    Eio_main.run @@ fun env ->
                    Cmd.eval (cmd env)

