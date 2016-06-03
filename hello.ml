open Core.Std
open Async.Std
open Cohttp_async

(* let test () = print_endline "hey"; *)



(* let handler ~body:_ _sock req = *)
(*   let uri = Cohttp.Request.uri req in *)
(*   let handle = *)
(*   match Uri.path uri with *)
(*   | "/test" -> *)
(*     Uri.get_query_param uri "hello" *)
(*     |> Option.map ~f:(fun v -> "hello: " ^ v ^ "\n") *)
(*     |> Option.value ~default:"No param hello supplied" *)
(*     |> Server.respond_with_string *)
(*   | _ -> *)
(*     Server.respond_with_string ~code:`Not_found "Route not found" *)
(*   in *)
(*   let x = *)
(*     (\* print_endline "hshs"; *\) *)
(*     match Uri.get_query_param uri "hello" with *)
(*     | Some b -> b *)
(*     | None -> "not found" *)
(*   in *)
(*   let begin_handle = *)
(*     x *)
(*     handle *)
(*   in begin_handle *)

(* let handler ~body:_ _sock req = *)
(*   let uri = Cohttp.Request.uri req in *)
(*   match Uri.path uri with *)
(*   | "/test" -> *)
(*     Uri.get_query_param uri "hello" *)
(*     |> Option.map ~f:(fun v -> "hello: " ^ v ^ "\n") *)
(*     |> Option.value ~default:"No param hello supplied" *)
(*     |> Server.respond_with_string *)
(*   | _ -> *)
(*     Server.respond_with_string ~code:`Not_found "Route not found" *)

let extract req =
  let uri = Cohttp.Request.uri req in
  match Uri.get_query_param uri "hello" with
  | Some(hello) -> if hello = "" then  None else Some hello
  | None -> None

let handler ~body:_ _sock req =
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with
  | "/test" ->
      extract req
      |> (fun x -> match x with
          | Some x -> print_endline x
            |> fun _ ->
            Server.respond_with_string "bye\n"
            (* Cohttp_async.Server.close _sock *)
            (* |> (fun _ ->  Server.respond_with_string "home") *)
          | _ -> Server.respond_with_string "default \n"
          (* | None -> Server.respond_with_string "None" *))
  | _ ->
    Server.respond_with_string ~code:`Not_found "Route not found\n"
  (* | _ -> return None *)

let start_server port () =
  eprintf "Listening for HTTP on port %d\n" port;
  eprintf "Try 'curl http://localhost:%d/test?hello=xyz'\n%!" port;
  (* let server = *)
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Tcp.on_port port) handler

  (* >>= fun addr -> Cohttp_async.Server.close addr *)
  >>= fun _ -> let dd = print_endline "bye"
  in
  Deferred.never
    dd

  (* ignore (server : (Socket.Address.Inet.t, int) Cohttp_async.Server.t Deferred.t) *)

(* Cohttp_async.Server.c *)
    (* Deferred.never () *)

let () =
  Command.async
    ~summary:"Start a hello world Async server"
    Command.Spec.(empty +>
                  flag "-p" (optional_with_default 8080 int)
                    ~doc:"int Source port to listen on"
                 ) start_server
  |> Command.run


