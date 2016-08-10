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

exception Diagram_not_found
exception Code_not_found
  
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

let diagram_node_id = ref None

let handle_diagram_id data = (* Maybe check diagram ref is not set first*)
  match !diagram_node_id with
  | None ->
    get_json_id data
    |> fun id -> diagram_node_id := Some id;
    id
  | Some x -> x
  (* !diagram_id *)

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

let branch_id_table = Hashtbl.create 30
(* let t = Hashtbl.add branch_id_table 1 "hy" *)

exception Diagram_id_not_found
let new_branch counter (* parent *) (* diagram *) text levels =
    if counter = 0 then new_diagram text
      >>= fun data ->
      let diagram_id = handle_diagram_id data in
      get_node_resource_id diagram_id
    else
      match !diagram_node_id with
      | None -> raise Diagram_id_not_found
      | Some diagram ->
        add_branch (Hashtbl.find branch_id_table (levels - 1))
          diagram text "55" "98"
        >>= fun body ->
        Cohttp_async.Body.to_string body
        >>| fun b -> get_json_id b
      (* >>= fun str_b -> (\* return (get_json_id str_b) *\) *)
      (* match Deferred.peek (Cohttp_async.Body.to_string body) with *)
      (* | None -> "" *)
      (* | Some x -> get_json_id x *)
     (* after this need to store the id returned *)
     (* |> get_json_id  *)


let tail_list ls =
  match (List.tl ls) with
  | None -> []
  | Some l -> l

exception Syntax_incorrect
(* let read_tokens tokens levels_count itr_count f = *)
(*   match (List.hd tokens) with *)
(*   | Some ")" -> if itr_count = 0 *)
(*     then raise (Syntax_incorrect) *)
(*       (\* print_endline "Syntax error unexpect )" *\) *)
(*     else let tail = tail_list tokens in *)
(*       let levels = levels_count - 1 in (\* minus count for closed paren *\) *)
(*       Hashtbl.remove branch_id_table levels_count; (\* id is no longer needed *\) *)
(*       f tail levels (itr_count + 1) *)
(*   | Some "(" -> *)
(*     let tail = tail_list tokens in *)
(*     let levels = levels_count + 1 in *)
(*     f tail levels (itr_count + 1) *)
(*   | Some token -> *)
(*     let diagram = match !diagram_node_id with *)
(*       | Some id -> id *)
(*       | None -> raise (Diagram_not_found ) in *)
(*     new_branch itr_count diagram token levels_count *)
(*       |> fun x -> (\* print_endline "hshs" *\) x *)
(*     (\* >>= fun x -> return (Hashtbl.add branch_id_table levels_count x) *\) *)
(*       (\* print_endline "hshs" *\) *)
(*     (\* >>| fun _ -> "jjsjs" *\) *)
(*     (\* |> fun _ -> return "hshs" *\) *)
(*     (\* >>= fun _ -> print_endline "hshs" *\) *)
(*   | None -> raise (Syntax_incorrect) (\* print_endline "nothing" *\) *)

let get_x d = match (Deferred.peek d) with
  | None -> " "
  | Some x -> x

exception Token_not_found
let rec tokens_parser tokens levels_count itr_count =
  match tokens with
  | [] -> if itr_count = 0
    then raise (Token_not_found)
    else (* return (Some "Parse Completed.") *) return ""
  (* | [hd] -> print_endline "call funct" (\* should call a parser *\) *)
  (* | first :: rest -> print_endline "hww" *)
  | tk_list -> (* read_tokens tokens levels_count itr_count tokens_parser *)
    match (List.hd tokens) with
    | Some ")" -> if itr_count = 0
      then raise (Syntax_incorrect)
      (* print_endline "Syntax error unexpect )" *)
      else let tail = tail_list tokens in
        let levels = levels_count - 1 in (* minus count for closed paren *)
        Hashtbl.remove branch_id_table levels_count; (* id is no longer needed *)
        tokens_parser tail levels (itr_count + 1)
    | Some "(" ->
      let tail = tail_list tokens in
      let levels = levels_count + 1 in
      tokens_parser tail levels (itr_count + 1)
    | Some token ->
      (* let diagram = match !diagram_node_id with *)
      (*   | Some id -> id *)
      (*   | None -> raise (Diagram_not_found ) *)
      (* in *) (* probably not needed as new_branch creates new diagram *)
      new_branch itr_count (* !diagram_node_id *) token levels_count
    (* |> fun x -> (Some x) *)
    | None -> raise (Syntax_incorrect) (* print_endline "nothing" *)

let get_coggle_token code =
  let headers = (Cohttp.Header.of_list create_auth) in
  Cohttp_async.Client.post_form
    ~headers: headers
    ~params:[("code", [code]);
             ("grant_type", ["authorization_code"]);
             ("redirect_uri", ["http://localhost:8080/coggle"])]
    (Uri.of_string "https://coggle.it/token")
    >>= fun (_, body) ->
    Cohttp_async.Body.to_string body
    >>| fun b -> (* changed from >>= *)
    (* printf "yup:  %s\n" b; *)
    (* parse_token b |> print_endline; *)
    parse_token b
    (* |> fun x ->  match (Deferred.peek x) with *)
    (* | None -> "" *)
    (* | Some d -> d *)
    |> fun tk ->
    tkn := tk;
    print_endline ("set: " ^ !tkn);
    tk
    (* new_diagram "testing" (\* make change here *\) *)
    (* >>= fun (diagram_data) -> *)


    
    (* let diagram_id = get_json_id diagram_data in (\* returns string id *\) *)
    (* get_all_nodes diagram_id *)
    (* >>= fun nodes -> *)
    (* (\* print_endline ("MY NODES: " ^ nodes); *\) *)
    (* String.slice nodes 1 (String.length nodes - 1) *)
    (* |> get_json_id *)

    
    (* let diagram_id = handle_diagram_id diagram_data in *)
    (* get_node_resource_id diagram_id *)
    (* >>= fun id -> *)
    (* add_branch id diagram_id "does this work" "32" "10" *)



let extract req =
  let uri = Cohttp.Request.uri req in
  match Uri.get_query_param uri "code" with
  | Some(code) ->
    if code = "" then
      None (* false *)
    else
      (* print_endline code *)
      (* printf "Code: %s \n" get_secret *)
      (* get_coggle_token code *)
      (* |> fun _ -> Some code *)
      (* |> fun _ ->  true *)
      Some code
  | None -> None


let init req =
  extract req
  |> fun code ->
  match code with
  | None -> raise (Code_not_found )
  | Some code ->
    get_coggle_token code
    |> fun _ -> tokens_parser

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
         (* extract req *)
         init req
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


