open Core.Std
open Async.Std
open Cohttp_async
open Async_ssl.Std
open Sexplib.Std
(* open Yojson *)
(* open Cohttp_lwt_unix *)
open Str


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

let tkn = ref ""

let new_diagram title =
  let uri = Uri.of_string "https://coggle.it/api/1/diagrams" in
  Cohttp_async.Client.post_form
    ~params: [("access_token", [!tkn]);
              ("title", [title])]
    uri
  >>= fun (_, body) ->
  Cohttp_async.Body.to_string body
  >>= fun b -> print_endline ("Diagram: " ^ b);
  (* return body *)
  return b
  (* >>= fun data -> print_endline data; *)
  (* return data *)


(* let input_data_commandline *)
(* let parse data = *)


let get_all_nodes diagram =
  let uri = Uri.of_string ("https://coggle.it/api/1/diagrams/" ^ diagram ^ "/nodes?access_token=" ^ !tkn) in
  Cohttp_async.Client.get uri
    >>= fun (_, body) ->
    Cohttp_async.Body.to_string body
    >>= fun b ->
    print_endline ("NODES: " ^ b);
    return b
    (* return body (\* was body *\) *)

let get_json_id data =
  let json = Yojson.Basic.from_string data in
  let open Yojson.Basic.Util in
  json |> member "_id" |> to_string


(* let add_branch parent text x y = *)
(*   let uri = Uri.of_string *)
(*       ("https://coggle.it/api/1/diagrams/" ^ parent ^ "/nodes") in *)
(*   let js = `Assoc [ ("x", `String "12"); ("y", `String "43") ] in *)
(*   Cohttp_async.Client.post_form *)
(*     ~params: [("access_token", [!tkn]); *)
(*               ("offset",  ["[x: 5, y: 4]"]); *)
(*               (\* ("offset",  [("x", x) ("y", y)]) *\) *)
(*               (\* ("x", [x]); *\) *)
(*               (\* ("y", [y]); *\) *)
(*               ("text", [text]); *)
(*               ("parent", [parent])] *)
(*     uri *)
(*       >>= fun (_, body) -> *)
(*       Cohttp_async.Body.to_string body *)
(*       >>= fun b -> print_endline b; *)
(*       return body *)


(* Cohttp_async.Body.of_string *)

let add_branch parent diagram text x y =
  let headers = Cohttp.Header.of_list [("content-type", "application/json")] in
  let uri = Uri.of_string
      ("https://coggle.it/api/1/diagrams/" ^ diagram ^ "/nodes?access_token=" ^ !tkn)
  in
  let data  = `Assoc [("offset", `Assoc [("x", `Int 12); ("y", `Int 20)]);
                      ("text", `String text);
                      ("parent", `String parent)] in
  let main_body = Cohttp_async.Body.of_string (Yojson.Basic.to_string data)  in
  Cohttp_async.Client.post
    ~headers: headers
    ~body:main_body
    uri
    >>= fun (_, body) ->
    Cohttp_async.Body.to_string body
    >>= fun b -> print_endline b;
    return body

(* Yojson.Basic. *)

(* let add_branch parent text x y = *)
(*   let uri = Uri.of_string *)
(*       ("https://coggle.it/api/1/diagrams/:diagram/nodes?access_token=" ^ !tkn ^ "&parent=" ^ parent) in *)
(*   Cohttp_async.Client.post_form *)
(*     ~params: [(\* ("access_token", [!tkn]); *\) *)
(*               ("x", [x]); *)
(*               ("y", [y]); *)
(*               ("text", [text]); *)
(*               (\* ("parent", [parent]) *\)] *)
(*     uri *)
(*       >>= fun (_, body) -> *)
(*       Cohttp_async.Body.to_string body *)
(*       >>= fun b -> print_endline b; *)
(*       return body *)



let parse_token str =
  let json = Yojson.Basic.from_string str in
  let open Yojson.Basic.Util in
  let tkn = json |> member "access_token" |> to_string in
  (* print_endline ("Tkn: " ^ tkn) *)
  (* printf "Tkn: %s\n" tkn *)
  tkn


let create_auth =
  let client_id = "5748885591ce2c8246852e66" in
  let auth = client_id ^ ":" ^ get_secret in
  let enc = B64.encode auth in
  let auth_headers = ["Authorization", "Basic " ^ enc] in
  auth_headers

let generate_uri code =
  let uri = Uri.of_string "https://coggle.it/token" in
  Uri.add_query_params uri[
    ("code", [code]);
    ("grant_type", ["authorization_code"]);
    ("redirect_uri", ["http://localhost:8080/coggle"])]

let replace old _new str  =
  Str.global_replace (Str.regexp old) _new str

let tokenize code =
  replace "(" " ( " code
  |> replace ")" " ) "
  (* |> fun y -> String.slice y 1 (String.length y - 1) *)
  |> fun x -> String.split x ~on: ' '
              |> fun l -> List.filter l (fun s -> s <> "")

let diagram_id = ref None

let handle_diagram_id data = (* Maybe check diagram ref is not set first*)
  get_json_id data
  |> fun id -> diagram_id := Some id;
  (* !diagram_id *)
  id

let get_node_resource_id id =
  get_all_nodes id
  >>| fun nodes ->
  String.slice nodes 1 (String.length nodes - 1)
  |> get_json_id


(* let rec read_tokens tokens = *)
(*   match (String.length tokens) > 0 with *)
(*   | false -> failwith "Unexpected EOF while reading" *)
(*   | true -> let token = List.hd tokens in *)
(*     match token with *)
(*     | "(" -> let l = [] in *)
(*       while List.nth tokens 1 <> Some ")" *)
(*     | ")" -> failwith "Unexpected )" *)
(*     | _ -> *)


let get_token code =
  let headers = (Cohttp.Header.of_list create_auth) in
  Cohttp_async.Client.post_form
    ~headers: headers
    ~params:[("code", [code]);
             ("grant_type", ["authorization_code"]);
             ("redirect_uri", ["http://localhost:8080/coggle"])]
    (Uri.of_string "https://coggle.it/token")
    >>= fun (_, body) ->
    Cohttp_async.Body.to_string body
    >>= fun b ->
    (* printf "yup:  %s\n" b; *)
    (* parse_token b |> print_endline; *)
    parse_token b
    |> fun tk ->
    tkn := tk;
    print_endline ("set: " ^ !tkn);
    new_diagram "testing"
    >>= fun (diagram_data) ->
    (* let diagram_id = get_json_id diagram_data in (\* returns string id *\) *)
    (* get_all_nodes diagram_id *)
    (* >>= fun nodes -> *)
    (* (\* print_endline ("MY NODES: " ^ nodes); *\) *)
    (* String.slice nodes 1 (String.length nodes - 1) *)
    (* |> get_json_id *)
    let diagram_id = handle_diagram_id diagram_data in
    get_node_resource_id diagram_id
    >>= fun id ->
    add_branch id diagram_id "does this work" "32" "10"
    (* return nodes *)


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


