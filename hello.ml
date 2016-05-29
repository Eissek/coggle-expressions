open Async.Std
open Core.Std
open Cohttp_async


let handler ~body:_ _sock req =
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with
  | "/test" ->
    Uri.get_query_param uri "hello"
      |> Option.map ~f:(fun v -> "hello: " ^ v)
      |> Option.value ~default:"No param hello supplied"
      |> Server.respond_with_string
  | _ ->
    Server.respond_with_string ~code:`Not_found "Route not found"


