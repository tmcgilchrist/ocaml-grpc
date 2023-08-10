let main _server_host _server_host_override _server_port _test_case _use_tls _use_test_ca _default_service_account _oauth_scope _service_account_key_file _service_config_json _additional_metadata = Eio.traceln "Implement me"

(* Command-line parsing *)

open Cmdliner

let server_host =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"The server host to connect to. For example, 'localhost' or '127.0.0.1'" ~docv:"HOSTNAME" [ "server_host" ]

let server_host_override =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"The server host to claim to be connecting to, for use in TLS and HTTP/2 :authority header. If unspecified, the value of --server_host will be used" ~docv:"HOSTNAME" [ "server_host_override" ]

let server_port =
  Arg.value
  @@ Arg.opt Arg.(some int) None
  @@ Arg.info ~doc:"The server port to connect to. For example, '8080'" ~docv:"PORT" [ "server_port" ]

let test_case =
  Arg.value
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

let cmd =
  let doc = "Client for interop tests" in
  let info = Cmd.info "client-interop" ~doc in
  Cmd.v info
    Term.(const main $ server_host $ server_host_override $ server_port $ test_case $ use_tls $ use_test_ca $ default_service_account $ oauth_scope $ service_account_key_file $ service_config_json $ additional_metadata)

let () =
  exit @@
    Eio_main.run @@ fun _env ->
                    Cmd.eval cmd

