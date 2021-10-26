(*14-1*)
let l1 = `Nil
and l2 = `Cons(1, `Nil)
and l3 = `Cons(2, `Cons(1, `Nil));;

let rec append l1 l2 =
  match l1 with
    `Nil -> l2
  | `Cons(a, l) -> `Cons(a, append l l2);; 
append l2 l3;;

let rec map f = function
    `Nil -> `Nil
  | `Cons(a, l) -> `Cons(f a, map f l);; 
map (fun x -> x * 2) l3;;

let rec downto1 = function
    0 -> `Nil
  | a -> `Cons(a, downto1 (a-1));; 
downto1 6;;

(*14-2*)
let l6 = `Cons(1, `App(l2, l3));;
let rec list_of_alist = function
    `Nil -> `Nil
  | `Cons(a, l) -> `Cons(a, list_of_alist l)
  | `App (l1, l2) -> append (list_of_alist l1) (list_of_alist l2);;

list_of_alist l6;; 
