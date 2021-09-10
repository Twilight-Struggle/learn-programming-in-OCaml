(*5-6*)
let randlist = [77.; 102.; 14.;
                58.; 24.; 16.;
                111.; 52.; 37.; 1.];;

let rec quick_sort list result = 
  match list with
    [] -> result
  | pivot :: rest -> 
      let rec partition left right = function
          [] -> quick_sort left (pivot::quick_sort right result)
        | y :: ys -> if pivot < y then partition left (y :: right) ys 
            else partition (y :: left) right ys in
      partition [] [] rest;;

let quick_sort list = quick_sort list [];;

quick_sort randlist;;

(*5-7*)
let square r =
  let sqrted = int_of_float(sqrt(float_of_int(r)))+1 in
  let rec sq_in a b res =
    match (a, b) with
      (x, y) when x < 0 -> res
    | (x, y) when y < 0 -> sq_in (x-1) (x-1) res
    | (x, y) when x * x + y * y < r -> sq_in (x-1) (x-1) res
    | (x, y) when x * x + y * y = r -> sq_in (x-1) (x-1) ((x, y) :: res)
    | (x, y) -> sq_in x (y-1) res in
  sq_in sqrted sqrted [];; 

square 48612265;;

(*5-8*)
let rec old_map f = function
    (*[x] -> f x :: old_map f [x]*)
    [] -> []
  | x :: rest -> f x :: old_map f rest;;

let rev l =
  let rec revap l1 l2 =
    match l1 with
      [] -> l2
    | v :: rest -> revap rest (v::l2) in
  revap l [];;

let map f list =
  let rec mapin f res = function 
      [] -> rev res
    | x :: rest -> mapin f ((f x) :: res) rest in
  mapin f [] list;;
  
old_map (fun x -> x + 1) [1;2;3];;
map (fun x -> x + 1) [1;2;3];;