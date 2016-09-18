open Core.Std
open Async.Std
open Sexplib.Std

let branch_id_table = Hashtbl.create 30

(* let generate_position level rand = *)
(*   (\* Random.self_init (); *\) *)
(*   (level * Random.int 26 (\* 20 *\)) + Random.int rand *)

let generate_position level rand =
  let x = Random.int rand in
  match level with
  | 1 -> if x mod 2 = 0 then x * -15 else x
  | _ -> if x mod 2 = 0 then Random.int 16 * (-x)
    else
    ((* level *  *)Random.int 16) * x

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



let get_json_id data =
  let json = Yojson.Basic.from_string data in
  let open Yojson.Basic.Util in
  json |> member "_id" |> to_string

let new_diagram title tkn =
  let uri = Uri.of_string "https://coggle.it/api/1/diagrams" in
  Cohttp_async.Client.post_form
    ~params: [("access_token", [tkn]);
              ("title", [title])]
    uri
  >>= fun (_, body) ->
  Cohttp_async.Body.to_string body
  >>= fun b -> print_endline ("Diagram: " ^ b);
  (* return body *)
  return b

let add_branch parent diagram text levels tkn (* x y *) =
  let headers = Cohttp.Header.of_list [("content-type", "application/json")] in
  let uri = Uri.of_string
      ("https://coggle.it/api/1/diagrams/" ^ diagram ^ "/nodes?access_token=" ^ tkn)
  in
  let data  = `Assoc [(* ("offset", `Assoc [("x", `Int 100); ("y", `Int 70)]); *)
                      ("offset", `Assoc [("x", `Int (generate_position levels 25)); ("y", `Int (generate_position levels 20))]);
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


let diagram_node_id = ref None

let handle_diagram_id data = (* Maybe check diagram ref is not set first*)
  match !diagram_node_id with
  | None ->
    get_json_id data
    |> fun id -> diagram_node_id := Some id;
    id
  | Some x -> x
  (* !diagram_id *)


let get_all_nodes diagram tkn =
  let uri = Uri.of_string ("https://coggle.it/api/1/diagrams/" ^ diagram ^ "/nodes?access_token=" ^ tkn) in
  Cohttp_async.Client.get uri
    >>= fun (_, body) ->
    Cohttp_async.Body.to_string body
    >>= fun b ->
    print_endline ("NODES: " ^ b);
    return b
    (* return body (\* was body *\) *)

let get_node_resource_id id tkn =
  get_all_nodes id tkn
  >>| fun nodes ->
  String.slice nodes 1 (String.length nodes - 1)
  |> get_json_id

exception Diagram_id_not_found

let new_branch (* counter *) (* parent *) (* diagram *) text levels token_count tkn =
  print_endline "NEW BRANCH";
  (* print_int token_count; *)
  if token_count = 1 then
    new_diagram text tkn
    >>= fun data ->
    print_endline "NEW DIAGRAM";
    (* print_endline data; *)
    let diagram_id = handle_diagram_id data in
    get_node_resource_id diagram_id tkn
  else
    (match !diagram_node_id with
    | None ->
      raise Diagram_id_not_found
    | Some diagram when levels = 1 ->
      add_branch (Hashtbl.find branch_id_table levels)
        diagram text levels tkn (* "55" "98" *)
    | Some diagram ->
      (* print_endline "HEEEEEEEElp"; *)
      (* print_int (levels - 1); *)
      add_branch (Hashtbl.find branch_id_table (levels - 1))
          diagram text levels tkn (* "55" "98" *))
    >>= fun body ->
    (* Cohttp_async.Body.to_string body *)
    (* >>= fun b -> *)
    print_endline ("B: " ^ body);
    return (get_json_id body)
