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
exception Deferred_is_None



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


let generate_position level rand =
  (* Random.self_init (); *)
  (level * Random.int 26 (* 20 *)) + Random.int rand

(* let generate_position level rand = *)
(*   (level * (let r = Random.int 20 in *)
(*             match r mod 2 with *)
(*             | 0 -> r *)
(*             | _ -> r * -1)) + Random.int rand *)

(* let generate_position level rand = *)
(*   ((match level mod 2 with *)
(*     | 0 -> level *)
(*     | _ -> level * -3) *)
(*   * Random.int 20) + Random.int rand *)

let add_branch parent diagram text levels (* x y *) =
  let headers = Cohttp.Header.of_list [("content-type", "application/json")] in
  let uri = Uri.of_string
      ("https://coggle.it/api/1/diagrams/" ^ diagram ^ "/nodes?access_token=" ^ !tkn)
  in
  let data  = `Assoc [(* ("offset", `Assoc [("x", `Int 100); ("y", `Int 70)]); *)
                      ("offset", `Assoc [("x", `Int (generate_position levels 60)); ("y", `Int (generate_position levels 40))]);
                      ("text", `String text);
                      ("parent", `String parent)] in
  let main_body = Cohttp_async.Body.of_string (Yojson.Basic.to_string data)  in
  Cohttp_async.Client.post
    ~headers: headers
    ~body:main_body
    uri
    >>= fun (_, body) ->
    Cohttp_async.Body.to_string body
    >>= fun b -> (* print_endline b; *)
    (* return body *)
    return b


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


(* let insert_replaced_data begin_index close_index original_str sub_str = *)
(*   match (begin_index > 0) with *)
(*   | true -> *)
(*     let before = Str.string_before original_str begin_index in *)
(*     if close_index < (String.length original_str - 1) then *)
(*       let after = Str.string_after original_str close_index in *)
(*       String.concat [before; sub_str; after] *)
(*     else (\* reach the last index e.g. the end of the string *\) *)
(*       String.concat [before; sub_str] *)
(*   | false -> (\* starts at the very first index so nothing before it*\) *)
(*     if close_index < (String.length original_str - 1) then *)
(*       let after = Str.string_after original_str close_index in *)
(*       String.concat [sub_str; after] *)
(*     else *)
(*       sub_str *)

(* let rec replace_spaces index data = *)
(*   try *)
(*     let opening_quote = Str.search_forward (Str.regexp "\\\"") data index in *)
(*     let closing_quote = Str.search_forward (Str.regexp "\\\"") data (opening_quote + 1) in *)
(*     String.sub ~pos:opening_quote ~len:(closing_quote - 1) data *)
(*     |> replace " " "§" *)
(*     |> insert_replaced_data opening_quote closing_quote data *)
(*     |> replace_spaces (closing_quote + 1) (\* the last param is always added from the pipe*\) *)
(*    with *)
(*      Not_found -> data *)

(* let replace_spaces opening_quote closing_quote data f = *)
(*     String.sub ~pos:opening_quote ~len:(closing_quote - 1) data *)
(*     |> replace " " "§" *)
(*     |> insert_replaced_data opening_quote closing_quote data *)
(*     |> f (closing_quote + 1) (\* the last param is always added from the pipe*\) *)
(*    (\* with *\) *)
(*    (\*   Not_found -> data *\) *)

(* let rec find_quotes index data = *)
(*   try *)
(*     match (String.index_from data index '"') with *)
(*     | None -> data *)
(*     | Some x -> *)
(*       (match (String.index_from data (x + 1) '"') with *)
(*       | None -> data *)
(*       | Some y -> replace_spaces x y data find_quotes) *)
(*   with *)
(*   (\* | Invalid_argument s -> data *\) *)
(*   | Not_found -> data *)

let data_from_file = ref ""

let concat_remaining last_index len replaced_data =
  match (last_index < (len - 1)) with
  | false -> replaced_data
(* Nothing to concat as there is nothing after the last index*)
  | true when last_index = 0 -> replaced_data
  | true -> (* replaced_data ^ (Str.string_after !data_from_file last_index) *)
    String.concat [replaced_data; (Str.string_after !data_from_file last_index)]
(* "§" *)
let transverse_data data =
  let rec find_string_in_data start last count replaced_data =
    let len = String.length !data_from_file in
    match (String.index_from data start '"') with
    | None -> concat_remaining last len replaced_data
    | Some x ->
      (match (String.index_from data (x + 1) '"') with
       | None -> concat_remaining last len replaced_data
       | Some y ->
         let last_start_difference = (y-x) + 1 in
         let fill_space = replace " " "§" (String.sub ~pos:x ~len:last_start_difference data) in
         if x > 0 then
           let d = (if count = 0 then
                   String.concat [(String.sub ~pos:last ~len:(x - last) data); fill_space]
                   else
                   String.concat [replaced_data; (String.sub ~pos:last ~len:(x - last) data); fill_space])
           in
           find_string_in_data (y + 1) (y + 1) (count + 1) d
         else find_string_in_data (y + 1) (y + 1) (count + 1) fill_space);
  in
  find_string_in_data 0 0 0 !data_from_file




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


let new_branch (* counter *) (* parent *) (* diagram *) text levels token_count =
  print_endline "NEW BRANCH";
  (* print_int token_count; *)
  if token_count = 1 then
    new_diagram text
    >>= fun data ->
    print_endline "NEW DIAGRAM";
    (* print_endline data; *)
    let diagram_id = handle_diagram_id data in
    get_node_resource_id diagram_id
  else
    (match !diagram_node_id with
    | None ->
      raise Diagram_id_not_found
    | Some diagram when levels = 1 ->
      add_branch (Hashtbl.find branch_id_table levels)
        diagram text levels (* "55" "98" *)
    | Some diagram ->
      (* print_endline "HEEEEEEEElp"; *)
      (* print_int (levels - 1); *)
      add_branch (Hashtbl.find branch_id_table (levels - 1))
          diagram text levels (* "55" "98" *))
    >>= fun body ->
    (* Cohttp_async.Body.to_string body *)
    (* >>= fun b -> *)
    print_endline ("B: " ^ body);
    return (get_json_id body)


let tail_list ls =
  match (List.tl ls) with
  | None -> []
  | Some l -> l

exception Syntax_incorrect


let get_x d = match (Deferred.peek d) with
  | None -> " "
  | Some x -> x

let get_def x = match Deferred.peek x with
  | None -> raise (Deferred_is_None)
  | Some y -> y

exception No_levels_or_id_returned

let store_node_id id =
  (* id param is a tuple *)
    match id with
    | None -> raise (No_levels_or_id_returned)
    | Some (x, y) -> Hashtbl.add branch_id_table x y;
     Some x

let read_tokens tokens levels_count itr_count f tkn_count =
  match (List.hd tokens) with
  | Some ")" -> if itr_count = 0
    then raise (Syntax_incorrect)
    (* print_endline "Syntax error unexpect )" *)
    else let tail = tail_list tokens in
      let levels = levels_count - 1 in (* minus count for closed paren *)
      Hashtbl.remove branch_id_table levels_count; (* id is no longer needed *)
      f tail levels (itr_count + 1) tkn_count
  | Some "(" ->
    let tail = tail_list tokens in
    let levels = levels_count + 1 in
    f tail levels (itr_count + 1) tkn_count
  | Some token ->
    (* let diagram = match !diagram_node_id with *)
    (*   | Some id -> id *)
    (*   | None -> raise (Diagram_not_found ) *)
    (* in *) (* probably not needed as new_branch creates new diagram *)
    (* print_int itr_count; *)
    new_branch (* itr_count *) (* !diagram_node_id *) token levels_count (tkn_count + 1)
    >>= fun x ->
    store_node_id (Some (levels_count, x))
    |> fun _ -> f (tail_list tokens) levels_count (itr_count + 1) (tkn_count + 1)
      (* (Some (levels_count, x)) *)
  (* |> fun x -> (Some x) *)
  | None -> raise (Syntax_incorrect) (* print_endline "nothing" *)

exception Token_not_found
let rec tokens_parser tokens levels_count itr_count tkn_count =
  match tokens with
  | [] -> if itr_count = 0
    then raise (Token_not_found)
    else
      (* None here indicates tokens have successfully been pasrsed *)
      (* And the branches should have been created using read_tokens *)
      print_endline "Diagram created successfully";
      return None
      (* return (Some "Parse Completed.") *) (* return "" *)
  (* | [hd] -> print_endline "call funct" (\* should call a parser *\) *)
  (* | first :: rest -> print_endline "hww" *)
  | (* tk_list *) _ -> read_tokens tokens levels_count itr_count tokens_parser tkn_count

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


(* let test_c = ["("; "begin"; "("; "2nd"; "("; "2.2"; ")"; ")"; *)
(*               "("; "3rd"; ")"; ")"] *)


let init req tokens =
  Random.self_init ();
  extract req
  |> fun code ->
  match code with
  | None -> raise (Code_not_found )
  | Some code ->
    get_coggle_token code
    >>= fun _ -> tokens_parser tokens 0 0 0 (* test_c 0 0 0 *)
    (* |> store_node_id *)
(* fun x -> match x with *)
    (* | None -> raise (No_levels_or_id_returned) *)
    (* | Some (x, y) -> Hashtbl.add branch_id_table x y; x *)

let start_server port filename () =
  eprintf "Listening for HTTP on port %d\n" port;
  eprintf "Try 'curl http://localhost:%d/coggle?code=xyz'\n%!" port;
  print_endline "Enter the following url to your browser:";
  print_endline "https://coggle.it/dialog/authorize?response_type=code&scope=read write&client_id=5748885591ce2c8246852e66&redirect_uri=http://localhost:8080/coggle";
  (* let param = ref false in *)
  let inet_addr = ref None in
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Tcp.on_port port)
    (fun ~body: _ _sock req ->
       let uri = Cohttp.Request.uri req in
       match Uri.path uri with
       | "/coggle" ->
         (* extract req *)
         In_channel.read_all filename
         |> transverse_data
         |> tokenize
         |> List.filter ~f:(fun x -> x <> "\n")
         |> List.map ~f:(fun current -> replace  "§" " " current)
         |> init req (* test_c *)
           |> (fun _ ->
             match !inet_addr with
             | None -> Server.respond_with_string "inet address not found"
             | _ (* Some y *) -> (* Cohttp_async.Server.close y *)
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
    ~summary:"Start Async server"
    Command.Spec.(empty
                  +> flag "-p" (optional_with_default 8080 int)
                    ~doc:"int Source port to listen on"
                  +> anon ("filename" %: string)
                 ) start_server
  |> Command.run


