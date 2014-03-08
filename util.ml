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

let unixtime_to_human unixtime =
  let t = Unix.localtime unixtime in
  Printf.sprintf "%d%02d%02d.%02d%02d%02d" (t.Unix.tm_year + 1900) (t.Unix.tm_mon + 1) (t.Unix.tm_mday) (t.Unix.tm_hour) (t.Unix.tm_min) (t.Unix.tm_sec)

let to_int64 = function
  | `Int x    -> Int64.of_int x
  | `Intlit x -> Int64.of_string x
  | _	    -> assert false

let sort_assoc l = List.sort (fun a b -> compare (fst a) (fst b)) l

