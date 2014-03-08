open Printf
open Yojson.Safe

let db = Couchdb_lwt.mk_database (Couchdb_lwt.mk_server "172.17.42.1") "faucet"

let bitcointalk_doc user_id asset = sprintf "bitcointalk-%d-%s" user_id asset

let bitcointalk_json_data user_id username address timestamp asset amount = [("user_id", `Int user_id);("username", `String username);("address", `String address);("timestamp", `Intlit timestamp);("asset", `String asset);("amount", `Intlit amount)]

let now () = sprintf "%.0f" (Unix.time ())

let new_bitcointalk_beg user_id username address asset = 
  let json = to_string (`Assoc (bitcointalk_json_data user_id username address (now ()) asset "0")) in
  Couchdb_lwt.Basic.put_create db (bitcointalk_doc user_id asset) json

let get_beggars asset amount callback =
  let (view,key) = match amount with
      Some amount -> ("income", [("startkey", sprintf "[\"%s\",%s]" asset (Int64.to_string amount)); ("endkey", sprintf  "[\"%s\",0]" asset)])
    | None -> ("all", [("startkey",sprintf "[\"%s\",%s]" asset (now ())); ("endkey",sprintf "[\"%s\",0]" asset)])
  in
  Couchdb_lwt.View.search db ~params:(Some ([("include_docs","true");("descending","true")] @ key)) "beggar" view (
    function
      Couchdb_lwt.Result_list rows -> callback rows
    | Couchdb_lwt.Error cause -> callback []
    | _ -> callback []
  )
