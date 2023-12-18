open Grpc_eio
open Routeguide

(* $MDX part-begin=client-h2 *)
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
  H2_eio.Client.create_connection ~sw ~error_handler:ignore socket

(* $MDX part-end *)
(* $MDX part-begin=client-get-feature *)
let call_get_feature connection point =
  let response =
    Client.call ~service:"routeguide.RouteGuide" ~rpc:"GetFeature"
      ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Rpc.unary
           (let encoder = Pbrt.Encoder.create () in
            Route_guide.encode_pb_point point encoder;
            Pbrt.Encoder.to_string encoder
           )
           ~f:(function
             | Some response -> (
               let decoder = Pbrt.Decoder.of_string response in
               Route_guide.decode_pb_feature decoder)
             | None -> Route_guide.default_feature ()))
      ()
  in
  match response with
  | Ok (res, _ok) -> Format.(fprintf std_formatter "RESPONSE = {%a}" Route_guide.pp_feature res)
  | Error _ -> Format.(fprintf std_formatter "an error occurred")

(* $MDX part-end *)
(* $MDX part-begin=client-list-features *)
let print_features connection =
  let rectangle =
    let lo = Route_guide.default_point ~latitude:Int32.(of_int 400000000) ~longitude:(Int32.(of_int (-750000000))) () in
    let hi = Route_guide.default_point ~latitude:Int32.(of_int 420000000) ~longitude:Int32.(of_int (-730000000)) () in
    Route_guide.default_rectangle
      ~lo:(Some lo)
      ~hi:(Some hi)
      ()
  in

  let stream =
    Client.call ~service:"routeguide.RouteGuide" ~rpc:"ListFeatures"
      ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Rpc.server_streaming
           (
             let encoder = Pbrt.Encoder.create () in
             Route_guide.encode_pb_rectangle rectangle encoder;
             Pbrt.Encoder.to_string encoder)
           ~f:(fun responses ->
             let stream =
               Seq.map
                 (fun str ->
                   Pbrt.Decoder.of_string str
                   |> Route_guide.decode_pb_feature)
                 responses
             in
             stream))
      ()
  in
  match stream with
  | Ok (results, _ok) ->
      Seq.iter
        (fun f -> Format.(fprintf std_formatter "RESPONSE = {%a}" Route_guide.pp_feature f))
        results
  | Error e ->
      failwith (Format.sprintf "HTTP2 error: %s" (H2.Status.to_string e))

(* $MDX part-end *)
(* $MDX part-begin=client-random-point *)
let random_point () : Route_guide.point =
  let latitude = (Random.int 180 - 90) * 10000000 |> Int32.of_int in
  let longitude = (Random.int 360 - 180) * 10000000 |> Int32.of_int in
  Route_guide.default_point ~latitude ~longitude ()

(* $MDX part-end *)
(* $MDX part-begin=client-record-route *)
let run_record_route connection =
  let points =
    Random.int 100
    |> Seq.unfold (function 0 -> None | x -> Some (random_point (), x - 1))
  in

  let response =
    Client.call ~service:"routeguide.RouteGuide" ~rpc:"RecordRoute"
      ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Rpc.client_streaming ~f:(fun f response ->
             (* Stream points to server. *)
             Seq.iter
               (fun point ->
                 let encoder = Pbrt.Encoder.create () in
                 Route_guide.encode_pb_point point encoder;
                 Pbrt.Encoder.to_string encoder |> fun x -> Seq.write f x)
               points;

             (* Signal we have finished sending points. *)
             Seq.close_writer f;

             (* Decode RouteSummary responses. *)
             Eio.Promise.await response |> function
             | Some str -> (
               let decoder = Pbrt.Decoder.of_string str in
               Route_guide.decode_pb_route_summary decoder)
             | None -> failwith (Printf.sprintf "No RouteSummary received.")))
      ()
  in
  match response with
  | Ok (result, _ok) ->
      Format.(fprintf std_formatter "SUMMARY = {%a}" Route_guide.pp_route_summary result)
  | Error e ->
      failwith (Printf.sprintf "HTTP2 error: %s" (H2.Status.to_string e))

(* $MDX part-end *)
(* $MDX part-begin=client-route-chat-1 *)
let run_route_chat clock connection =
  (* Generate locations. *)
  let location_count = 5 in
  Printf.printf "Generating %i locations\n" location_count;
  let route_notes =
    location_count
    |> Seq.unfold (function
         | 0 -> None
         | x ->
             Some
               ( Route_guide.default_route_note ~location:(Some (random_point ()))
                   ~message:(Printf.sprintf "Random Message %i" x)
                   (),
                 x - 1 ))
  in
  (* $MDX part-end *)
  (* $MDX part-begin=client-route-chat-2 *)
  let rec go writer reader notes =
    match Seq.uncons notes with
    | None ->
        Seq.close_writer writer (* Signal no more notes from the client. *)
    | Some (route_note, xs) -> (
      let encoder = Pbrt.Encoder.create () in
      Route_guide.encode_pb_route_note route_note encoder;
      Pbrt.Encoder.to_string encoder |> fun x ->
                                        Seq.write writer x;

        (* Yield and sleep, waiting for server reply. *)
        Eio.Time.sleep clock 1.0;
        Eio.Fiber.yield ();

        match Seq.uncons reader with
        | None -> failwith "Expecting response"
        | Some (response, reader') ->
            let route_note =
              Pbrt.Decoder.of_string response
              |> Route_guide.decode_pb_route_note
            in
            Format.(fprintf std_formatter "NOTE = {%a}\n" Route_guide.pp_route_note route_note);
            go writer reader' xs)
  in
  let result =
    Client.call ~service:"routeguide.RouteGuide" ~rpc:"RouteChat"
      ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Rpc.bidirectional_streaming ~f:(fun writer reader ->
             go writer reader route_notes))
      ()
  in
  match result with
  | Ok ((), _ok) -> ()
  | Error e ->
      failwith (Printf.sprintf "HTTP2 error: %s" (H2.Status.to_string e))

(* $MDX part-end *)
(* $MDX part-begin=client-main *)

let main env =
  let port = "8080" in
  let host = "localhost" in
  let clock = Eio.Stdenv.clock env in
  let network = Eio.Stdenv.net env in
  let () = Random.self_init () in

  let run sw =
    let connection = client ~sw host port network in

    Printf.printf "*** SIMPLE RPC ***\n";
    let request =
      Route_guide.default_point ~latitude:(Int32.of_int 409146138) ~longitude:(Int32.of_int (-746188906)) ()
    in
    let result = call_get_feature connection request in

    Printf.printf "\n*** SERVER STREAMING ***\n";
    print_features connection;

    Printf.printf "\n*** CLIENT STREAMING ***\n";
    run_record_route connection;

    Printf.printf "\n*** BIDIRECTIONAL STREAMING ***\n";
    run_route_chat clock connection;

    Eio.Promise.await (H2_eio.Client.shutdown connection);
    result
  in

  Eio.Switch.run run

let () = Eio_main.run main

(* $MDX part-end *)
