(*7-1*)
let find onum olist =
  let rec find_in x = function
      [] -> raise Not_found
    | a :: l when a = x -> 1
    | _ :: l -> 1 + find_in x l in
  try Some (find_in onum olist) with Not_found -> None;;

find 7 [0;8;7;3];;
find 9 [0;8;7;3];;

(*7-2*)
let prod_list inlist =
  let rec prod_list_in = function
      [] -> 1
    | a :: rest when a = 0 -> raise (Invalid_argument "0")
    | a :: rest -> a * prod_list_in rest in
  try prod_list_in inlist with Invalid_argument "0" -> 0;;

prod_list [8;7;3] ;;
prod_list [0;8;7;3];;

(*7-3*)
let rec change coins amount =
  match (coins, amount) with
    (_, 0) -> []
  | ((c :: rest) as coins, total) -> if c > total then change rest total 
      else (try c :: change coins (total - c) with Failure "change"
          -> change rest total)
  | _ -> raise (Failure "change");;

change [5;2] 16;;
