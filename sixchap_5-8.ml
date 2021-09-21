(*6-5*)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

let rec comptree x n =
  if n = 1 then Br (x, Lf, Lf)
  else Br (x, comptree x (n-1), comptree x (n-1));;

let comptree' n =
  let rec comp x i=
    if i = n then Br (x, Lf, Lf)
    else Br (x, comp (2*x) (i+1), comp (2*x+1) (i+1)) in
  comp 1 1;;

(*6-6*)
let rec preord t l =
  match t with 
    Lf -> l 
  | Br( x, left, right) -> x :: (preord left (preord right l));;

let rec inord t l =
  match t with
    Lf -> l
  | Br (x, left, right) -> inord left (x :: (inord right l));;

let rec postord t l =
  match t with
    Lf -> l
  | Br (x, left, right) -> postord left (postord right (x :: l));;

inord (comptree' 3) [];;
postord (comptree' 3) [];;

(*6-7*)
let rec reflect = function
    Lf -> Lf
  | Br (x, left, right) -> Br (x, reflect right, reflect left);;

reflect (comptree' 3);;
preord (reflect (comptree' 3)) [];; (* = rev (postord t)*)
inord (reflect (comptree' 3)) [];; (* = rev (inord t)*)
postord (reflect (comptree' 3)) [];; (* = rev (preord t)*) 
                                                           
(*6-8*)
type 'a rosetree = RLf | RBr of 'a * 'a rosetree list;; 

let rec tree_of_rtree = function 
    RLf -> Br (None, Lf, Lf) 
  | RBr (a, rtrees) -> Br (Some a, tree_of_rtreelist rtrees, Lf)
and tree_of_rtreelist = function
    [] -> Lf
  | rtree :: rest -> let Br (a, left, Lf) = tree_of_rtree rtree in
      Br (a, left, tree_of_rtreelist rest);;

let rtree_of_tree treee = 
  let rec rtree_of_tree_in = function
      Br (None, Lf, Lf) -> [RLf]
    | Br (Some a, left, Lf) -> [RBr (a, rtree_of_tree_in left)]
    | Br (Some a, Lf, right)-> RBr (a, [RLf]) :: (rtree_of_tree_in right)
    | Br (None, Lf, right)-> RLf :: (rtree_of_tree_in right)
    | Br (Some a, left, right) -> RBr (a, rtree_of_tree_in left)
                                  :: (rtree_of_tree_in right) in
  let a :: rest = rtree_of_tree_in treee in a;;

let rtree = RBr ("a", 
                 [RBr ("b", [RBr ("c", [RLf]); RLf; RBr ("d", [RLf])]);
                  RBr ("e", [RLf]);
                  RBr ("f", [RLf])]);;

tree_of_rtree rtree;;
rtree_of_tree (tree_of_rtree rtree);;
