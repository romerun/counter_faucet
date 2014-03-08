open Printf
open Counterparty

let url = "http://localhost:8080/api/hungry_beggars?asset=XCP&amount=100000000";

module Ctp_connection =
struct
	let default = Some
		{
		inet_addr = Unix.inet_addr_loopback;
        host = "localhost";
		port = 4000;
		username = "rpcuser";
		password = "graffitiseis";
		}
end

module CTP = Counterparty.Make (Counterparty_ocamlnet.Httpclient) (Ctp_connection)

let process_beggar = ()
(*function
    [("user_id",`Int user_id);("username",`String username);("address",`String address);"timestamp":1394259350,"amount":0,"asset":"XCP"}*)

let rec poll () =
  let call = new Http_client.get url in
  let pipeline = new Http_client.pipeline in
  pipeline#add call;
  pipeline#run ();
  (
    match call#status with
    | `Successful ->
       let content = call#get_resp_body () in
       begin match Yojson.Safe.from_string content with
               `List beggars ->
               List.iter (function 
                             `Assoc assoc -> process_beggar (Util.sort_assoc assoc)
                         ) beggars
             | _ -> assert false;
       end
    | _ ->
       printf "%s is not available\n" url;
       flush_all();       
  );

  Unix.sleep 1;
  poll ()

let () = poll ()
