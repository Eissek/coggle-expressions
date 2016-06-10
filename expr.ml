open Core.Std
open Async.Std
open Cohttp_async
open Async_ssl.Std
(* open Cohttp_lwt_unix *)


(* let response_handler code = *)
(*   let token_url = "https://coggle.it/token?code=" ^ code ^  *)

(* let get_secret = *)
(*   let contents = Reader.file_contents "../moonlandings.txt" *)
(*       in let d = contents in d in  *)
(*               match (contents |> fun _ -> Deferred.peek contents) with *)
(*               | None -> "" *)
(*               | Some x ->  x *)

let get_id = "5748885591ce2c8246852e66"
let get_secret  = In_channel.read_all "../moonlandings.txt"
                    |> String.strip
(* let enc = B64.encode "hehehehe";; *)
let create_auth =
  let client_id = "5748885591ce2c8246852e66" in
  let auth = client_id ^ ":" ^ get_secret in
  (* let auth = get_secret in *)
  (* let auth =  get_secret ^ ":" ^ client_id in *)
  (* let auth = client_id ^ get_secret in *)
  (* let auth = "client_id=" ^ client_id ^ "&client_secret" ^ get_secret in *)
  let enc = B64.encode auth in
  let auth_headers = ["Authorization", "Basic " ^ enc] in
  auth_headers

let generate_uri code =
  (* let uri = Uri.of_string ("https://coggle.it/token?code=" ^ code *)
  (*                          ^ "&client_id=" ^ get_id ^ "&client_secret=" ^ get_secret ^ *)
  (*                          "&grant_type=authorization_code&redirect_uri=http://localhost:8080/coggle" ) in *)
  (* uri *)
  let uri = Uri.of_string "https://coggle.it/token" in
  Uri.add_query_params uri[
    ("code", [code]);
    (* ("client_id", [get_id]); *)
    (* ("client_secret", [(B64.encode get_secret )]); *)
    ("grant_type", ["authorization_code"]);
    ("redirect_uri", ["http://localhost:8080/coggle"])]

(* ("client_id", [B64.encode get_id]); *)
    (* ("client_secret", [B64.encode get_secret]); *)

(* Cohttp.Header.init_with *)
let get_token code =
  (* let client_id = "5748885591ce2c8246852e66" in *)
  (* let headers = Cohttp.Header.init_with "Authorization" ("Basic " ^ B64.encode (client_id ^ ":" ^ get_secret)) in *)
  let headers = (Cohttp.Header.of_list create_auth) in
  let uri = generate_uri code in
  Cohttp_async.Client.post_form
    ~headers: headers
    ~params:[("code", [code]);
             ("grant_type", ["authorization_code"]);
             ("redirect_uri", ["http://localhost:8080/coggle"])]
    (Uri.of_string "https://coggle.it/token")
    >>= fun (_, body) ->
    Cohttp_async.Body.to_string body
    >>= fun b ->
    printf "yup:  %s\n" b;
    return body
  (* Cohttp_async.Client.post ~headers:headers uri *)
  (*   >>= fun (_, body) -> *)
  (*   Cohttp_async.Body.to_string body *)
  (*   >>= fun b -> *)
  (*   printf "yup:  %s\n" b; *)
  (*   return body *)
    (* let d = Deferred.peek body in *)
    (* match d with *)
    (* | None -> None *)
    (* | Some x -> Some x *)


let extract req =
  let uri = Cohttp.Request.uri req in
  match Uri.get_query_param uri "code" with
  | Some(code) ->
    if code = "" then
      None
    else
      (* print_endline code *)
      (* printf "Code: %s \n" get_secret *)
      get_token code
      |> fun _ -> Some code
  | None -> None

(* Cohttp_async.Client.get *)
let start_server port () =
  eprintf "Listening for HTTP on port %d\n" port;
  eprintf "Try 'curl http://localhost:%d/coggle?code=xyz'\n%!" port;
  print_endline "Enter the following url to your browser:";
  print_endline "https://coggle.it/dialog/authorize?response_type=code&scope=read write&client_id=5748885591ce2c8246852e66&redirect_uri=http://localhost:8080/coggle";
  let param = ref false in
  let inet_addr = ref None in
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Tcp.on_port port)
    (fun ~body: _ _sock req ->
       let uri = Cohttp.Request.uri req in
       match Uri.path uri with
       | "/coggle" ->
         extract req
           |> (fun _ ->
             match !inet_addr with
             | None -> Server.respond_with_string "inet address not found"
             | Some y -> (* Cohttp_async.Server.close y *)
                         (* |> fun _ -> *)
                         print_endline "Received Authorization code";
                         print_endline "Closing server";
                         Server.respond_with_string "Received Authorization code \n")
       | _ -> Server.respond_with_string ~code:`Not_found "Route not found\n"
    )
  >>= fun addr -> let set_inet =
                    inet_addr := Some addr
  in Deferred.never
    set_inet


let () =
  Command.async
    ~summary:"Start a hello world Async server"
    Command.Spec.(empty +>
                  flag "-p" (optional_with_default 8080 int)
                    ~doc:"int Source port to listen on"
                 ) start_server
  |> Command.run


