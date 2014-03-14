open Printf
open Counterparty
open Json_data

let url = "http://xcp.bfolder.com/api/hungry_beggars?asset=XCP&amount=100000000&include_id=true"
let db = Couchdb_lwt.mk_database (Couchdb_lwt.mk_server "172.17.42.1") "faucet"

module Ctp_connection =
struct
	let default = Some
		{
		inet_addr = Unix.inet_addr_of_string "172.17.42.1";
	        host = "localhost";
		port = 4000;
		username = "rpcuser";
		password = "graffitiseis";
		}
end

module CTP = Counterparty.Make (Counterparty_ocamlnet.Httpclient) (Ctp_connection)

let process_beggar id assoc beggar =
  let credits = CTP.get_credits ~filters:[("asset",Filter.EQ,"XCP");("address",Filter.EQ,beggar.Beggar.address)] () in
  let total_credit = List.fold_left (fun acc x -> Int64.add acc x.Credit.amount) 0L credits in
  if Int64.compare beggar.Beggar.amount total_credit <> 0 then
    printf "updating credit of %s to %s\n" beggar.Beggar.username (Int64.to_string total_credit); flush_all ();

    let json = `Assoc (
                  List.map (fun (k,v) -> 
                            if k = "amount" then
                              (k,`Intlit (Int64.to_string total_credit))
                            else
                              (k,v)
                           ) assoc
                )
    in
    let json_string = Yojson.Safe.to_string json in
    let thr = Couchdb_lwt.Basic.update db id json_string (function Couchdb_lwt.Result_create _ -> Lwt.return () | _ -> eprintf "update fail - %s" json_string; flush_all(); Lwt.return ()) in
    Lwt_main.run thr

let rec poll () =
  let call = new Http_client.get url in
  let pipeline = new Http_client.pipeline in
  pipeline#add call;
  pipeline#run ();
  (
    match call#status with
    | `Successful ->
       let content = call#get_resp_body () in
       printf "%s\n" content; flush_all ();
       begin match Yojson.Safe.from_string content with
               `List beggars ->
               List.iter (function 
                             `Assoc assoc ->
                             begin match assoc with
                                   | (("_id",`String id)::("_rev",_)::tl) -> process_beggar id assoc (Beggar.to_beggar (Util.sort_assoc tl))
                                   | _ -> assert false;
                             end
                           | _ -> assert false;
                         ) beggars
             | _ -> assert false;
       end
    | _ ->
       printf "%s is not available\n" url;
       flush_all();       
  );

  Unix.sleep 60;
  poll ()

let () = poll ()
