let main _port _use_tls = Eio.traceln "Implement me"

(* Command-line parsing *)
open Cmdliner

let port =
  Arg.value
  @@ Arg.opt Arg.(some int) None
  @@ Arg.info ~doc:"The port to listen on. For example, '8080'" ~docv:"PORT" [ "port" ]

let use_tls =
  Arg.value
  @@ Arg.opt Arg.(some bool) None
  @@ Arg.info ~doc:"Whether to use a plaintext or encrypted connection"  [ "use_tls" ]

let cmd =
  let doc = "Server for interop tests" in
  let info = Cmd.info "server-interop" ~doc in
  Cmd.v info
    Term.(const main $ port $ use_tls)

let () =
  exit @@
    Eio_main.run @@ fun _env ->
                    Cmd.eval cmd

