{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
}}

open Cohttp_lwt_unix
open Lwt
open Printf
open Json_data

module Counter_faucet_app =
  Eliom_registration.App (
    struct
      let application_name = "counter_faucet"
    end)

let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let bitcointalk_service = 
  Eliom_service.App.post_service ~fallback:main_service ~post_params:Eliom_parameter.(int "user_id"**string "signature") ()

let message = "Counterparty A DISTRIBUTED FINANCIAL MARKET"

let bitcointalk_form = Eliom_content.Html5.F.Unsafe.data (sprintf "
<p>[XCP] <a href='https://www.counterparty.co/resources/'>Counterparty</a> is a protocol built on top of Bitcoin network. In order to use it, you will need to have some XCP tokens.</p>

<p>You could either</p>
<ul>
<li>buy some from <a href='https://www.poloniex.com/exchange/btc_xcp'>Poloniex</a></li>
<li>buy some trustlessly from <a href='http://www.blockscan.com/order_book.aspx'>counterparty distributed exchange</a> with BTC</li>
<li>convert BTC to <a href='https://forums.counterparty.co/index.php/topic,160.msg1107'>XBTC</a> where you have to trust the gateway who issues XBTC, and then buy XCP with XBTC on counterparty distributed exchange</li>
<li>or get some here from our faucet for free</li>
</ul>

<h2>Bitcointalk</h2>
<p><a href='/beggars?asset=XCP'>check status</a></p>
<p>Condition: require Bitcointalk account (registered before Jan 1st 2014 or has activity at least 20)</p>

<p>Enter BTC address, that you want to receive XCP (this address must never have XCP on it), on <a href='/images/ss1.png'>your Bitcointalk profile</a>, then <a href='/images/ss2.png'>sign message</a> \"%s\" with that address.</p>

<form action='/' method='POST'>
<p>bitcointalk user id: <input name='user_id' /></p>
<p>signature: <input name='signature' size='88' /></p>
<input type='submit' value='submit' />
</form>
" message)

let html_template ?(headers=[]) body_content =
  (html
     (head (title (pcdata "Counterparty faucet"))
           headers
     )
     (body body_content)
  )

let send_error err =
  Lwt.return (html_template [h1 [a ~service:main_service ~xhr:false [pcdata err] ()]])

let bitcointalk_username_pat = Str.regexp "Name: </b></td>[^<]+?<td>\\([^<]+\\)</td>"
let bitcointalk_address_pat = Str.regexp "Bitcoin address: </b></td>[^<]+?<td>\\([^<]+\\)</td>"
let bitcointalk_date_registered_pat = Str.regexp "Date Registered: </b></td>[^<]+?<td>\\([^<]+\\)</td>"
let year_pat = Str.regexp "\\(20[012][0123456789]\\)"
let bitcointalk_activity_pat = Str.regexp "Activity:</b></td>[^<]+?<td>\\([^<]+\\)</td>"

let () =
  ignore(Counter_faucet_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"counter_faucet"
           ~css:[["css";"counter_faucet.css"]]
           Html5.F.(body [
             bitcointalk_form
    ]))));

  ignore(Eliom_registration.Html_text.register_service
    ~path:["api";"hungry_beggars"]
    ~content_type:"application/json"
    ~get_params:Eliom_parameter.(string "asset"**string "amount"**bool "include_id")
    (fun (asset,(amount,include_id)) () ->
     let amount = Int64.of_string amount in
     Db.get_beggars asset (Some amount) (fun list ->
       let list = List.map (function
                               `Assoc [("id",_);("key",_);("value",_);("doc",`Assoc assoc)] ->
                               if include_id then
                                 `Assoc assoc
                               else
                                 begin match assoc with
                                       | (("_id",_)::("_rev",_)::tl) -> `Assoc tl
                                       | _ -> assert false
                                 end
                             | _ -> assert false
                           ) list in
       Lwt.return (Yojson.Safe.to_string (`List list))
       )
    ));

  ignore(Eliom_registration.Html5.register_service
    ~path:["beggars"]
    ~get_params:Eliom_parameter.(string "asset")
    (fun asset () ->
     let display beggar =
         li [pcdata (sprintf "%s: %s - %s - %s %s donated" (Util.unixtime_to_human beggar.Beggar.timestamp) beggar.Beggar.username beggar.Beggar.address (Util.string_of_satoshi beggar.Beggar.amount) beggar.Beggar.asset)]
     in
     Db.get_beggars asset None (fun list ->
       let list = List.map (function
                               `Assoc [("id",_);("key",_);("value",_);("doc",`Assoc (("_id",_)::("_rev",_)::tl))] -> display (Beggar.to_beggar (Util.sort_assoc tl))
                             | _ -> assert false
                           ) list in
       Lwt.return
         (html
            (head (title (pcdata "All beggars status")) [])
            (body [ul list]))
       )
    ));

  ignore(Counter_faucet_app.register
    ~service:bitcointalk_service
    (fun () (user_id,signature) ->
     if String.length signature = 88 then
       Client.call ~chunked:false `GET (Uri.of_string (sprintf "https://bitcointalk.org/index.php?action=profile;u=%d" user_id)) >>= 
         fun (res, rbody) -> 
         bind (Cohttp_lwt_body.to_string rbody) 
              (fun s ->

               let error = send_error "BTC Address is not found on such bitcointalk account" in

               try
                 ignore(Str.search_forward bitcointalk_date_registered_pat s 0);
                 let date_registered  = Str.matched_group 1 s in
                 ignore(Str.search_forward year_pat date_registered 0);
                 let date_registered = int_of_string (Str.matched_group 1 date_registered) in
                 ignore(Str.search_forward bitcointalk_activity_pat s 0);
                 let activity = int_of_string(Str.matched_group 1 s) in

                 if date_registered >= 2014 && activity < 20 then
                   send_error "account appears to be created after 2013, please wait until your bitcointalk activity is at least 20"
                 else
                   begin
                     ignore(Str.search_forward bitcointalk_username_pat s 0);
                     let username = Str.matched_group 1 s in
                     ignore(Str.search_forward bitcointalk_address_pat s 0);
                     let btc_addr = Str.matched_group 1 s in

                     if String.length btc_addr = 34 && String.sub btc_addr 0 1 = "1" then
                       Coin.BtcService.validateaddress btc_addr >>=
                         function
                         | None -> send_error "BTC Address in your profile appears to be invalid"
                         | Some _ ->
                            Coin.BtcService.verifymessage btc_addr signature message >>=
                              function
                              | true ->
                                 Db.new_bitcointalk_beg user_id username btc_addr "XCP" (
                                                          function 
                                                          | Couchdb_lwt.Result_create _ -> 
                                                             let str = Eliom_content.Html5.F.Unsafe.data (sprintf "We will be sending some XCP to you by soon-ish, you'll see it at <a target='_blank' href='http://blockscan.com/balance.aspx?q=%s'>http://blockscan.com/balance.aspx?q=%s</a> once it's sent" btc_addr btc_addr) in
                                                             Lwt.return (html_template [str])
                                                          | _ -> send_error "duplicate entry"
                                                     )
                              | false -> send_error "wrong signature"
                     else
                       send_error (sprintf "BTC Address in your profile appears to be invalid -- %s" btc_addr)
                   end
               with
                 Not_found -> error
              )
     else
       send_error "definitely wrong signature"
    ));
