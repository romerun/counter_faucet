module Btc_connection =
struct
  let default = Some
		{
		  Bitcoin.inet_addr = Unix.inet_addr_loopback;
		  host = "localhost";
		  port = 8332;
		  username = "bitcoinrpc";
		  password = "J1qPr3m2YR8jqXGFRAii8XkGXiM6t1pcckZ1HNN9VRBw";
		}
end

module BtcService = Bitcoin.Make (Bitcoin_ocsigen.Httpclient) (Btc_connection)
