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

let bitcointalk_min_activity = 100

let message = "Counterparty A DISTRIBUTED FINANCIAL MARKET"

let bitcointalk_form = Eliom_content.Html5.F.Unsafe.data (sprintf "
<style>
* {
  margin-top: 0px;
}

img.logo {
  margin-bottom: 10px;
}

div.giveaway {
  border-color: green;
  border-width: 3px;
  border-style: dotted;
  padding: 5px;
}

div.faq {
  border-color: red;
  border-width: 1px;
  border-style: solid;
  padding: 5px;
}

body { background-color: #E6E6E9; }
</style>

<img class='logo' src='/images/logo.png' width='90%%'/>
<p><a href='https://bitcointalk.org/index.php?topic=395761.0;topicseen'>[XCP]</a> - <a href='https://www.counterparty.co/resources/'>Counterparty</a> is a protocol built on top of Bitcoin network. Since XCP resides on BTC blockchain, XCP address = BTC address, you don't need to run a separate wallet to generate XCP address.</p>

<p>Of course, in order to use Counterparty network, you will need to have some XCP tokens.</p>

<p>You could either</p>
<ul>
<li>buy some from <a href='https://www.poloniex.com/exchange/btc_xcp'>Poloniex</a> or <a href='https://bter.com/trade/xcp_btc'>Bter</a></li>
<li>buy some trustlessly from <a href='http://www.blockscan.com/order_book.aspx'>counterparty distributed exchange</a> with BTC</li>
<li>convert BTC to <a href='https://forums.counterparty.co/index.php/topic,160.msg1107'>XBTC</a> where you have to trust the gateway who issues XBTC, and then buy XCP with XBTC on counterparty distributed exchange</li>
<li>or get some here from our faucet for free. </li>
</ul>
<center><img src='/images/please.png' /></center>
<div class='giveaway'>
<h2>Bitcointalk</h2>
<p><a href='/beggars?asset=XCP'>check status</a></p>

<p>Enter BTC address, that you want to receive XCP (this address must never have XCP on it), on <a href='/images/ss1.png'>your Bitcointalk profile</a>, then <a href='/images/ss2.png'>sign message</a> \"%s\" with that address.</p>

<form action='/' method='POST'>
<p>bitcointalk user id: <input name='user_id' /> (not username)</p>
<p>signature: <input name='signature' size='88' /> <input type='submit' value='submit' /> </p>
</form>

<div class='faq'>
<p>Condition: require Bitcointalk account (registered before Jan 1st 2014 or has activity at least %d)</p>
<p>How much do I get ?: Our faucet works in a distributed manner, meaning that any XCP holders can run <a href='https://github.com/romerun/counter_faucet/blob/master/script/donate.py'>the script</a> to pull the list of eligible addresses to send out the giveaway himself. You may get a lot or you may get none, but nothing to lose except a tiny bit of time.</p>
</div>

</div>

<center><img src='/images/donator.png' /></center>

<div class='giveaway'>
<h2>Donate XCP</h2>
<p>You can either run <a href='https://github.com/romerun/counter_faucet/blob/master/script/donate.py'>this script</a> to give some XCP to the list of eligible people yourself.</p>
<p>or if you trust me and are not so keen to run anything, you could send some XCP to the following addresses, and I help distributing them</p>
<ul>
<li>1HGT1utMx3JbkDyrCiH3rf84FzK1BVEhSm <a href='http://blockscan.com/address.aspx?q=1HGT1utMx3JbkDyrCiH3rf84FzK1BVEhSm'>blockscan</a></li>
<li>1NhGMimWGD37EDVVQiy8xfjXer72ywFTHB <a href='http://blockscan.com/address.aspx?q=1NhGMimWGD37EDVVQiy8xfjXer72ywFTHB'>blockscan</a></li>
<li>13LVzzK1wEGH51gyn3BPga2wtaYf7v3eWP <a href='http://blockscan.com/address.aspx?q=13LVzzK1wEGH51gyn3BPga2wtaYf7v3eWP'>blockscan</a></li>
</ul>
</div>

" message bitcointalk_min_activity)

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
           ~title:"Counterparty faucet"
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
       let sockpuppet = if beggar.Beggar.sockpuppet then "(likely sock puppet)" else "" in
       li [Eliom_content.Html5.F.Unsafe.data 
             (sprintf "%s: %s -  <a target='_blank' href='http://blockscan.com/balance.aspx?q=%s'>%s</a> - %s %s donated %s" 
                      (Util.unixtime_to_human beggar.Beggar.timestamp)
                      beggar.Beggar.username beggar.Beggar.address beggar.Beggar.address
                      (Util.string_of_satoshi beggar.Beggar.amount)
                      beggar.Beggar.asset sockpuppet
          )]
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

                 if date_registered >= 2014 && activity < bitcointalk_min_activity then
                   send_error (Printf.sprintf "account appears to be created after 2013, please wait until your bitcointalk activity is at least %d" bitcointalk_min_activity)
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
