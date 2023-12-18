open Grpc_eio
open Routeguide

(* Derived data types to make reading JSON data easier. *)
type location = { latitude : int; longitude : int } [@@deriving yojson]
type feature = { location : location; name : string } [@@deriving yojson]
type feature_list = feature list [@@deriving yojson]

let features : Route_guide.feature list ref = ref []

let compare_point (p1 : Route_guide.point) (p2 : Route_guide.point) =
  p1.latitude == p2.latitude &&
    p1.longitude == p2.longitude

module RouteNotesMap = Hashtbl.Make (struct
  type t = Route_guide.point

  let equal = compare_point
  let hash s = Hashtbl.hash s
end)

(** Load route_guide data from a JSON file. *)
let load path : Route_guide.feature list =
  let json = Yojson.Safe.from_file path in
  match feature_list_of_yojson json with
  | Ok v ->
      List.map
        (fun feature ->
          Route_guide.default_feature ~name:feature.name
            ~location:
            (Some (Route_guide.default_point ~longitude:(Int32.of_int feature.location.longitude)
                 ~latitude:(Int32.of_int feature.location.latitude) ()))
            ())
        v
  | Error err -> failwith err

let in_range (point : Route_guide.point) (rect : Route_guide.rectangle) : bool =
  let lo = Option.get rect.lo in
  let hi = Option.get rect.hi in

  let left = Int32.min lo.longitude hi.longitude in
  let right = Int32.max lo.longitude hi.longitude in
  let top = Int32.max lo.latitude hi.latitude in
  let bottom = Int32.min lo.latitude hi.latitude in

  point.longitude >= left && point.longitude <= right
  && point.latitude >= bottom && point.latitude <= top

let pi = 4. *. atan 1.
let radians_of_degrees = ( *. ) (pi /. 180.)

(* Calculates the distance between two points using the "haversine" formula. *)
(* This code was taken from http://www.movable-type.co.uk/scripts/latlong.html. *)
let calc_distance (p1 : Route_guide.point) (p2 : Route_guide.point) : int =
  let cord_factor = 1e7 in
  let r = 6_371_000.0 in
  (* meters *)
  let lat1 = Float.of_int (Int32.to_int p1.latitude) /. cord_factor in
  let lat2 = Float.of_int (Int32.to_int p2.latitude) /. cord_factor in
  let lng1 = Float.of_int (Int32.to_int p1.longitude) /. cord_factor in
  let lng2 = Float.of_int (Int32.to_int p2.longitude) /. cord_factor in

  let lat_rad1 = radians_of_degrees lat1 in
  let lat_rad2 = radians_of_degrees lat2 in

  let delta_lat = radians_of_degrees (lat2 -. lat1) in
  let delta_lng = radians_of_degrees (lng2 -. lng1) in

  let a =
    (sin (delta_lat /. 2.0) *. sin (delta_lat /. 2.0))
    +. cos lat_rad1 *. cos lat_rad2
       *. sin (delta_lng /. 2.0)
       *. sin (delta_lng /. 2.0)
  in
  let c = 2.0 *. atan2 (sqrt a) (sqrt (1.0 -. a)) in
  Float.to_int (r *. c)

(* $MDX part-begin=server-get-feature *)
let get_feature (buffer : string) =
  (* Decode the request. *)
  let point = Pbrt.Decoder.of_string buffer |> Route_guide.decode_pb_point in
  Eio.traceln "GetFeature = {:%a}" Route_guide.pp_point point;

  (* Lookup the feature and if found return it. *)
  let feature =
    List.find_opt
      (fun (f : Route_guide.feature) ->
        match (f.location, point) with
        | Some p1, p2 -> compare_point p1 p2
        | _, _ -> false)
      !features
  in
  Eio.traceln "Found feature %a" (Format.pp_print_option Route_guide.pp_feature) feature;
  match feature with
  | Some feature ->
     (Grpc.Status.(v OK), Some (let encoder = Pbrt.Encoder.create () in
                                Route_guide.encode_pb_feature feature encoder;
                                Pbrt.Encoder.to_string encoder))
  | None ->
      (* No feature was found, return an unnamed feature. *)
      ( Grpc.Status.(v OK),
        Some (
            let feature = Route_guide.default_feature ~location:(Some point) () in
            let encoder = Pbrt.Encoder.create () in
            Route_guide.encode_pb_feature feature encoder;
            Pbrt.Encoder.to_string encoder))

(* $MDX part-end *)
(* $MDX part-begin=server-list-features *)
let list_features (buffer : string) (f : string -> unit) =
  (* Decode request. *)
  let rectangle =
    Pbrt.Decoder.of_string buffer |> Route_guide.decode_pb_rectangle
  in

  (* Lookup and reply with features found. *)
  let () =
    List.iter
      (fun (feature : Route_guide.feature) ->
        if in_range (Option.get feature.location) rectangle then
          (let encoder = Pbrt.Encoder.create () in
           Route_guide.encode_pb_feature feature encoder;
           Pbrt.Encoder.to_string encoder |> f)
         else ())
      !features
  in
  Grpc.Status.(v OK)

(* $MDX part-end *)
(* $MDX part-begin=server-record-route *)
let record_route (clock : _ Eio.Time.clock) (stream : string Seq.t) =
  Eio.traceln "RecordRoute";

  let last_point = ref None in
  let start = Eio.Time.now clock in

  let point_count, feature_count, distance =
    Seq.fold_left
      (fun (point_count, feature_count, distance) i ->
        let point =
          Pbrt.Decoder.of_string i |> Route_guide.decode_pb_point
        in
        Eio.traceln "  ==> Point = {%a}" Route_guide.pp_point point;

        (* Increment the point count *)
        let point_count = point_count + 1 in

        (* Find features *)
        let feature_count =
          List.find_all
            (fun (feature : Route_guide.feature) ->
              compare_point (Option.get feature.location) point)
            !features
          |> fun x -> List.length x + feature_count
        in

        (* Calculate the distance *)
        let distance =
          match !last_point with
          | Some last_point -> calc_distance last_point point
          | None -> distance
        in
        last_point := Some point;
        (point_count, feature_count, distance))
      (0, 0, 0) stream
  in
  let stop = Eio.Time.now clock in
  let elapsed_time = int_of_float (stop -. start) in
  let summary =
    Route_guide.default_route_summary ~point_count:(Int32.of_int point_count)
      ~feature_count:(Int32.of_int feature_count) ~distance:(Int32.of_int distance)
      ~elapsed_time:(Int32.of_int elapsed_time) ()
  in
  Eio.traceln "RecordRoute exit\n";
  (Grpc.Status.(v OK), Some (
                           Pbrt.Encoder.create ()
                           |> fun encoder -> Route_guide.encode_pb_route_summary summary encoder
                           |> fun () -> Pbrt.Encoder.to_string encoder))

(* $MDX part-end *)
(* $MDX part-begin=server-route-chat *)
let route_chat (stream : string Seq.t) (f : string -> unit) =
  Printf.printf "RouteChat\n";

  Seq.iter
    (fun i ->
      let note =
        Pbrt.Decoder.of_string i |> Route_guide.decode_pb_route_note
      in
      Format.(fprintf std_formatter "  ==> Note = {%a}\n" Route_guide.pp_route_note note);
      let encoder = Pbrt.Encoder.create () in
      Route_guide.encode_pb_route_note note encoder;
      Pbrt.Encoder.to_string encoder |> f)
    stream;

  Printf.printf "RouteChat exit\n";
  Grpc.Status.(v OK)

(* $MDX part-end *)
(* $MDX part-begin=server-grpc *)
let route_guide_service clock =
  Server.Service.(
    v ()
    |> add_rpc ~name:"GetFeature" ~rpc:(Unary get_feature)
    |> add_rpc ~name:"ListFeatures" ~rpc:(Server_streaming list_features)
    |> add_rpc ~name:"RecordRoute" ~rpc:(Client_streaming (record_route clock))
    |> add_rpc ~name:"RouteChat" ~rpc:(Bidirectional_streaming route_chat)
    |> handle_request)

let server clock =
  Server.(
    v ()
    |> add_service ~name:"routeguide.RouteGuide"
         ~service:(route_guide_service clock))

(* $MDX part-end *)
let connection_handler server ~sw =
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
      ~error_handler addr socket ~sw

(* $MDX part-begin=server-main *)
let serve server env =
  let port = 8080 in
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  Eio.Switch.run @@ fun sw ->
  let handler = connection_handler ~sw (server clock) in
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

let () =
  let path =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else failwith "Path to datafile required."
  in

  (* Load features. *)
  features := load path;

  Eio_main.run (serve server)
(* $MDX part-end *)
