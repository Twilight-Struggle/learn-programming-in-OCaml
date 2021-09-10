(*5-3-1*)
let rec mem a = function
    [] -> false
  | t :: rest -> if a = t then true else mem a rest;;

let mem_test = [1;2;3;4;5];;

mem 1 mem_test;;

(*5-3-2*)
let rec intersect s1 s2 =
  match s1 with
    [] -> []
  | a :: rest -> if mem a s2 then a :: intersect rest s2
      else intersect rest s2;;

let intersect_test = [2;4;5;6;8];;

intersect mem_test intersect_test;;

(*5-3-3*)
let rec union s1 s2 =
  match s1 with
    [] -> s2
  | a::rest -> if mem a s2 then union rest s2
      else a::union rest s2;;

union mem_test intersect_test;;

(*5-3-4*)
let rec diff s1 s2 = 
  let inter = intersect s1 s2 in
  match s1 with
    [] -> []
  | a::rest -> if mem a inter then diff rest s2
      else a::diff rest s2;;

diff mem_test intersect_test;;

(*5-4*)
let f x = 2 * x;;
let g x = x + 1;;

List.map f (List.map g [1;2;3;4;5]);;
List.map (fun x -> f (g x)) [1;2;3;4;5];;

(*5-5*)
let rec fold_right f l e =
  match l with
    [] -> e
  | x::rest -> f x (fold_right f rest e);;

let concat l = fold_right (fun x y -> x @ y) l [];;
concat [[0;3;4];[2];[];[5;0]];;

let forall f l = fold_right (fun x y -> f x && y) l true;;
forall (fun x -> x >= 5) [9;20;5];;
forall (fun x -> x >= 5) [6;3;9];; 

let exists f l = fold_right (fun x y -> f x || y) l false;;
exists (fun x -> (x mod 7) = 0) [23;98;19;53];;
