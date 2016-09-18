open Core.Std


let get_id = "5748885591ce2c8246852e66"
let get_secret  = In_channel.read_all "../moonlandings.txt"
                  |> String.strip

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

let parse_token str =
  let json = Yojson.Basic.from_string str in
  let open Yojson.Basic.Util in
  let tkn = json |> member "access_token" |> to_string in
  (* print_endline ("Tkn: " ^ tkn) *)
  (* printf "Tkn: %s\n" tkn *)
  tkn
