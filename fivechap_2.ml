(*5-2-1*)
let downto1 n =
  let rec init n l =
    match n with
      1 -> n :: []
    | _ -> n :: init (n-1) l in
  init n [];;

downto1 6;;

(*5-2-2*)
let roman_naive_input = 
  [(1000, "M"); (500, "D"); (100, "C"); (50, "L");
   (10, "X"); (5, "V"); (1, "I")];;

let roman_input = 
  [(1000, "M"); (900, "CM"); (500, "D"); (400, "CD"); 
   (100, "C"); (90, "XC"); (50, "L");(40, "XL");
   (10, "X"); (9, "IX"); (5, "V"); (4, "IV"); (1, "I")];;

let rec roman dict inp =
  match dict with
    [] -> ""
  | (a, b) :: rest -> if inp - a >= 0 then b ^ roman dict (inp - a)
      else roman rest inp;;

roman roman_naive_input  1984;;
roman roman_input 1984;;

(*5-2-3*)
let rec nested_length = 
  function
    [] -> 0
  | alist :: rest -> List.length alist + nested_length rest;;

nested_length [[1;2;3];[4;5];[6];[7;8;9;10]];;

(*5-2-4*)
let rec concat = function
    [] -> []
  | alist :: rest -> alist @ concat rest;;

concat [[0;3;4];[2];[];[5;0]];;

(*5-2-5*)
let rec zip alist blist =
  match (alist, blist) with
    ([], _) | (_, []) -> []
  | (a :: arest, b :: brest) -> (a, b) :: zip arest brest;;

let zipped = zip [2;3;4;5;6;7;8;9;10;11]
    [true; true; false; true; false; true; false; false; false; true];;

(*5-2-6*)
let rec unzip = function
    [] -> ([], [])
  | (a, b) :: rest -> let alist, blist = unzip rest in (a :: alist, b :: blist);;

unzip zipped;;

let rec filter func = function
    [] -> []
  | a :: rest -> if func a then a :: filter func rest else filter func rest;;

let is_positive x = (x > 0);;

(*5-2-7*)
filter is_positive [-9; 0; 2; 5; -3];;

filter (fun l -> List.length l = 3) [[1;2;3];[4;5];[6;7;8;];[9]];;

(*5-2-8*)
let rec take n = function
    [] -> []
  | a :: rest -> if n >= 1 then a :: take (n-1) rest else [];;

take 8 (downto1 10);;

let rec drop n = function
    [] -> []
  | a :: rest -> if n >= 1 then drop (n-1) rest else a :: drop (n-1) rest;;

drop 7 (downto1 10);;

(*5-2-9*)
let max_list inlist =
  let init :: outrest = inlist in
  let rec max_list_in n = function
      [] -> n
    | a :: rest -> if n > a then max_list_in n rest
        else max_list_in a rest in
  max_list_in init outrest;;

max_list [7;9;0;-5];;