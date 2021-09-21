(*6-3*)
type nat = Zero | OneMoreThan of nat;;

let zero = Zero and two = OneMoreThan (OneMoreThan Zero)
and three = OneMoreThan (OneMoreThan (OneMoreThan Zero));;
let rec add m n = 
  match m with 
    Zero -> n
  | OneMoreThan m' -> OneMoreThan (add m' n);;

let rec mul m n =
  match m with
    Zero -> Zero
  | OneMoreThan m' -> add n (mul m' n);;

mul two three;;

let rec monus m n =
  match (m, n) with
    (Zero, _) -> Zero
  | (_, Zero) -> m
  | (OneMoreThan m', OneMoreThan n') -> monus m' n';;

monus three two;;
monus two three;;

(*6-4*) 
let rec minus m n =
  match (m, n) with
    (Zero, Zero) -> Some Zero
  | (Zero, _) -> None
  | (_, Zero) -> Some m
  | (OneMoreThan m', OneMoreThan n') -> minus m' n';;

minus three two;;
minus two three;;