module Beggar =
struct
  type t = {address: string; amount: int64; asset: string; sockpuppet: bool; timestamp: float; user_id: int; username: string}

  let to_beggar = function
    [("address", `String address);("amount", amount);("asset", `String asset);("sockpuppet", `Bool sockpuppet);("timestamp", `Int timestamp);("user_id", `Int user_id);("username", `String username)] ->
    {address = address; amount = Util.to_int64 amount; asset = asset; sockpuppet = sockpuppet; timestamp = float_of_int timestamp; user_id = user_id; username=username}
    | [("address", `String address);("amount", amount);("asset", `String asset);("timestamp", `Int timestamp);("user_id", `Int user_id);("username", `String username)] ->
       {address = address; amount = Util.to_int64 amount; asset = asset; sockpuppet = false; timestamp = float_of_int timestamp; user_id = user_id; username=username}
    | _ -> assert false
end





