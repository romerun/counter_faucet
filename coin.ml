module Btc_connection =
struct
  let default = Some
		{
		  Bitcoin.inet_addr = Unix.inet_addr_of_string "172.17.42.1";
		  host = "localhost";
		  port = 8332;
		  username = "bitcoinrpc";
		  password = "J1qPr3m2YR8jqXGFRAii8XkGXiM6t1pcckZ1HNN9VRBw";
		}
end

module BtcService = Bitcoin.Make (Bitcoin_ocsigen.Httpclient) (Btc_connection)

module Testcoin_connection =
struct
	let default = Some
		{
		Counterparty.inet_addr = Unix.inet_addr_of_string "172.17.42.1";
        host = "localhost";
		port = 14000;
		username = "rpcuser";
		password = "graffitiseis";
		}
end

module Testcoin = Counterparty.Make (Counterparty_ocsigen.Httpclient) (Testcoin_connection)


