(* CouchDB Client implements a client for the CouchDB Document storage *)

open Yojson.Safe
open Lwt
open Cohttp_lwt_unix

let debug = false

let default_couchdb_port = 5984

type request_result_t = Network_fail of string | Internal_fail of string | Fail of string*string | Success of json

type end_result_t = Error of string | Result_bool of bool | Result_int of int | Result_create of string*string | Result_list of json list | Result_json of json

type insert_t = POST_INSERT of string | PUT_INSERT of (string*string)

type t = {
  hostname: string;
  port: int
}

type db = {
  server: t;
  database: string
}

type doc_id = string

let mk_server
    ?(port = default_couchdb_port)
    host =
  {
    hostname = host;
    port = port
  }

let mk_doc_id i = i

let mk_database server db =
  let valid_db name =
    let valid_db_names = Str.regexp "^[a-z0-9_$()+-/]+$" in
      Str.string_match valid_db_names name 0 in
    if valid_db db
    then
      { server = server;
        database = db }
    else
      raise (Invalid_argument "invalid_db")

module Util =
struct
  let unquote_string s =
    if String.length s > 0 && String.get s 0 = '"' then
      String.sub s 1 ((String.length s) -2)
    else
      s

  let satoshi_of_float f =
    let str = Printf.sprintf "%.09f" f in
    let hd = String.sub str 0 (String.length str - 10) in
    let tl = String.sub str (String.length str - 9) 8 in
    Int64.of_string (hd ^ tl)

  let rec prepend_0 s n =
    if n > 0 then
      prepend_0 ("0" ^ s) (n-1)
    else
      s

  let rec remove_0 s =
    let len = String.length s in
    if s.[len-1] = '0' && s.[len-2] <> '.' then
      remove_0 (String.sub s 0 (len-1))
    else
      s

  let string_of_satoshi l =
    let negative = Int64.compare l 0L = -1 in
    let l = Int64.abs l in
    let l = Int64.to_string l in
    let len = String.length l in
    let str = prepend_0 l (9-len) in
    let hd = String.sub str 0 (String.length str - 8) in
    let tl = String.sub str (String.length str - 8) 8 in
    let str = hd ^ "." ^ tl in
    let str = remove_0 str in
    if negative then "-" ^ str else str

end

module Request =
struct
  let build_url host port db components params =
    let s = Printf.sprintf "http://%s:%d/%s/%s" host port db (String.concat "/" components) in
    match params with
      None -> s
    | Some params -> let params = String.concat "&" (List.map (fun (k,v) -> Printf.sprintf "%s=%s" k v) params) in
      Printf.sprintf "%s?%s" s params

  let make_net_req ?(headers =[]) ?(body = None) meth url callback =
    (*Printf.printf "URL %s\n" url;
    flush_all ();*)

    let base_headers = [("Accept", "application/json"); ("Content-Type","application/json"); ("Connection","close")] in
    let headers = base_headers @ headers in
    let headers = Cohttp.Header.of_list headers in
      try_lwt
        Client.call ~chunked: false ~headers ?body:body meth (Uri.of_string url) >>= fun (res, rbody) -> bind (Cohttp_lwt_body.to_string rbody) (fun s ->
          if debug then (Printf.printf "***%s***\n" s; flush_all());

          try
            let json = from_string s in

            begin
              match json with
                `Assoc [("ok",`Bool false);("error", `String error); ("reason", `String reason)] -> callback (Fail (error,reason))
              | _ -> callback (Success json)
            end
          with
            | _ -> callback (Internal_fail (Printf.sprintf "failed to parse json: %s" s));
        )
      with
        exn -> callback (Network_fail (Printexc.to_string exn))

  let with_db ?(headers =[]) ?(body=None) ?(params = None) db m components callback =
    let {
      server = {
        hostname = host;
        port = port
      };

      database = db
    } = db in
      make_net_req ~headers ~body m (build_url host port db components params) callback
end

module Database =
struct
  exception DatabaseError of (int * string)
  let create db callback =
    Request.with_db db `PUT [] callback

  let create_ok db callback=
    let _ = create db in (* TODO: Fixme! *)
      ()

  let delete db callback =
    Request.with_db db `DELETE [] callback

  let info db callback =
    Request.with_db db `GET [] callback

  let compact db callback =
    Request.with_db db `POST ["_compact"] callback

  let list db callback =
    Request.with_db db `GET  ["_all_dbs"] callback
end

module Basic =
struct
  let get db doc_id callback =
    Request.with_db db `GET [doc_id] callback

  let create_result callback json = function
    | Network_fail cause | Internal_fail cause  -> callback (Error cause)
    | Fail (error,reason) -> Printf.eprintf "%s\n" json; flush_all(); callback (Error (Printf.sprintf "error - %s / %s" error reason))
    | Success (`Assoc (("ok", `Bool true)::("id", `String id)::("rev", `String rev)::_)) -> callback (Result_create (id,rev))
    | Success json -> callback (Error (Printf.sprintf "parse json error %s" (Yojson.Safe.to_string json)))

  let put_create db doc_id json callback = Request.with_db db `PUT ~body:(Some (Cohttp_lwt_body.of_string json)) [doc_id] (create_result callback json)

  let update db doc_id json callback = Request.with_db db `PUT ~body:(Some (Cohttp_lwt_body.of_string json)) [doc_id] (create_result callback json)

(*  let create db json callback =
    (*Printf.eprintf "create - %s\n" json; flush_all ();*)

    Request.with_db db `POST ~body:(Some (Cohttp_lwt_body.of_string json)) [] ( function
        Success (`Assoc ("id", `String id)::("rev", `String rev)::_) -> callback (Result_create (id,rev))
      | Network_fail cause | Internal_fail cause  -> callback (Error cause)
      | Fail (error,reason) -> callback (Error (Printf.sprintf "error - %s / %s" error reason))
    )

  let optimistic_create db json callback =
    Request.with_db db `POST ~body:(Some (Cohttp_lwt_body.of_string json)) [] ( function
        Success (`Assoc ("id", `String id)::("rev", `String rev)::_) -> callback (Result_create (id,rev))
      | Network_fail cause | Internal_fail cause  -> raise (Failure cause)
      | Fail (error,reason) -> raise (Failure (Printf.sprintf "error - %s / %s" error reason))
    )

  let delete db doc_id rev_id callback =
    Request.with_db db `DELETE [doc_id] ~params:(Some [("rev", rev_id)]) ( function
      Success json -> callback (Result_bool true)
    | Network_fail cause | Internal_fail cause  -> callback (Error cause)
    | Fail (error,reason) -> callback (Error (Printf.sprintf "error - %s / %s" error reason))
    )
 *)

(*
  let rec insert_transactions db acc callback = (function
    tx::tl ->
      begin
        let do_it =
          (function
              Result_create (id,rev) -> insert_transactions db ((id,rev)::acc) callback tl
            | Error err -> Printf.eprintf "post_transactions error: %s\n" err; List.iter (fun (id,rev) -> Printf.eprintf "(%s,%s)\n" id rev) acc; flush_all (); callback false
            | _ -> raise (Failure "typo")
          )
        in
        match tx with
          POST_INSERT s ->
            create db s do_it
        | PUT_INSERT (id,s) ->
            put_create db id s do_it
      end
    | [] -> callback true
  )*)
end

module View =
struct

  let view db design name callback =
    Request.with_db db `GET ["_design"; design; "_view"; name ] callback

  let search ?(params=None) db design name callback =
    Request.with_db db `GET ["_design"; design; "_view"; name ] ~params (function
      | Network_fail cause | Internal_fail cause -> callback (Error cause)
      | Fail (error,reason) -> callback (Error (Printf.sprintf "error - %s / %s" error reason))
      | Success (`Assoc [("total_rows",_);("offset",_);("rows", `List list)]) -> callback (Result_list list)
      | Success json -> callback (Error (Printf.sprintf "parse json error %s" (Yojson.Safe.to_string json)))
    )

(*  let search_raw ?(params=None) db design name callback =
    Request.with_db db `GET ["_design"; design; "_view"; name ] ~params (function
        Success json -> callback (Result_list Yojson.Basic.Util.member "rows" json)))
      | Network_fail cause | Internal_fail cause -> callback (Error cause)
      | Fail (error,reason) -> callback (Error (Printf.sprintf "error - %s / %s" error reason))
    )

  let search_one ?(params=None) db design name callback =
    Request.with_db db `GET ["_design"; design; "_view"; name ]  ~params (function
        Success json -> begin match (Yojson.Basic.Util.to_list (Yojson.Basic.Util.member "rows" json)) with row :: [] -> callback (Result_json row) | _ -> callback (Result_bool false) end
      | Network_fail cause | Internal_fail cause -> callback (Error cause)
      | Fail (error,reason) -> callback (Error (Printf.sprintf "error - %s / %s" error reason))
    )

  let search_one_nostrict ?(params=None) db design name callback =
    Request.with_db db `GET ["_design"; design; "_view"; name ]  ~params (function
        Success json -> begin match (Yojson.Basic.Util.to_list (Yojson.Basic.Util.member "rows" json)) with row :: _ -> callback (Result_json row) | [] -> callback (Result_bool false) end
      | Network_fail cause | Internal_fail cause -> callback (Error cause)
      | Fail (error,reason) -> callback (Error (Printf.sprintf "error - %s / %s" error reason))
    )

  let exists ?(params=None) db design name callback =
    Request.with_db db `GET ["_design"; design; "_view"; name ]  ~params (function
        Success json -> begin match (Yojson.Basic.Util.to_list (Yojson.Basic.Util.member "rows" json)) with [] -> callback (Result_bool false) | _ -> callback (Result_bool true) end
      | Network_fail cause | Internal_fail cause -> callback (Error cause)
      | Fail (error,reason) -> callback (Error (Printf.sprintf "error - %s / %s" error reason))
    )*)

end
