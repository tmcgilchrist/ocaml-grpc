open Grpc_eio

let empty_call buffer =
  let open Ocaml_protoc_plugin in
  let open Interop_proto.Test.Grpc.Testing in

  let decode, encode = Service.make_service_functions TestService.emptyCall in
  (* Decode the request. *)
  let () = Reader.create buffer
           |> decode
           |> function
             | Ok v -> v
             | Error e ->  failwith
                             (Printf.sprintf "Could not decode request: %s" (Result.show_error e))
  in
  (Grpc.Status.(v OK), Some (TestService.EmptyCall.Response.make ()|> encode |> Writer.contents))

let server =
  let service =
    Server.Service.(
      v()
      |> add_rpc ~name:"EmptyCall" ~rpc:(Unary empty_call)
      |> handle_request)
  in
  Server.(v ()
          |> add_service ~name:"grpc.testing.TestService" ~service)

let connection_handler server sw =
  let error_handler client_address ?request:_ _error start_response =
    Eio.traceln "Error in request from:%a" Eio.Net.Sockaddr.pp client_address;
    let response_body = start_response H2.Headers.empty in
    H2.Body.Writer.write_string response_body
      "There was an error handling your request.\n";
    H2.Body.Writer.close response_body
  in
  let request_handler _client_address request_descriptor =
    Eio.Fiber.fork ~sw (fun () ->
        Grpc_eio.Server.handle_request server request_descriptor)
  in
  fun socket addr ->
    H2_eio.Server.create_connection_handler ?config:None ~request_handler
      ~error_handler addr
      (socket :> Eio.Flow.two_way)

let main env port _use_tls =
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  Eio.Switch.run @@ fun sw ->
  let handler = connection_handler server sw in
  let server_socket =
    Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:10 addr
  in
  let rec listen () =
    Eio.Net.accept_fork ~sw server_socket
      ~on_error:(fun exn -> Eio.traceln "%s" (Printexc.to_string exn))
      handler;
    listen ()
  in
  Eio.traceln "Listening on port %i for grpc requests\n" port;
  listen ()

(* Command-line parsing *)
open Cmdliner

let port =
  Arg.value
  @@ Arg.opt Arg.int 10000
  @@ Arg.info ~doc:"The port to listen on. For example, '8080'" ~docv:"PORT" [ "port" ]

let use_tls =
  Arg.value
  @@ Arg.opt Arg.(some bool) None
  @@ Arg.info ~doc:"Whether to use a plaintext or encrypted connection"  [ "use_tls" ]

let cmd env =
  let doc = "Server for interop tests" in
  let info = Cmd.info "server-interop" ~doc in
  Cmd.v info
    Term.(const main $ (const env) $ port $ use_tls)

let () =
  exit @@
    Eio_main.run @@ fun env ->
                    Cmd.eval (cmd env)