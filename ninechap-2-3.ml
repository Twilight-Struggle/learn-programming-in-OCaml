(*9-2*)
module type TABLE2 =
sig
  type ('a, 'b) t (* = 以下 を 削除 *)
  val empty : ('a, 'b) t
  val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
  val retrieve : 'a -> ('a, 'b) t -> 'b option
  val dump : ('a, 'b) t -> ('a * 'b) list
end;;

module Binarytree : TABLE2 =
struct
  type ('a, 'b) t = Lf | Br of 'a * 'b * ('a, 'b) t * ('a, 'b) t
  let empty = Lf
  let rec retrieve key = function
      Lf -> None
    | Br (x, datum, left, right) ->
        if key = x then Some datum
        else if key < x then retrieve key left else retrieve key right
  let rec add key datum = function 
      Lf -> Br (key, datum, Lf, Lf)
    | (Br (x, y, left, right) as whole) when key = x -> whole
    | Br (x, y, left, right) when key < x -> Br(x, y, add key datum left, right)
    | Br (x, y, left, right) -> Br(x, y, left, add key datum right)
  let rec dump = function
      Lf -> []
    | Br (x, y, left, right) -> (x, y) :: ((dump left) @ (dump right))
end;;

open Binarytree;;
let bt = add 2 "Two" empty;;
let bt = add 1 "One" bt;;
let bt = add 3 "Three" bt;;
retrieve 1 bt;;
dump bt;;

module type QUEUE =
sig 
  type 'a t
  exception Empty 
  val empty: 'a t 
  val add: 'a t -> 'a -> 'a t
  val take: 'a t -> 'a * 'a t
  val peek: 'a t -> 'a
end;;

module Queue1 : QUEUE =
struct
  type 'a t = 'a list
  exception Empty
  let empty = []
  let add l a = l @ [a]
  let take = function 
      [] -> raise Empty
    | a :: rest -> (a, rest)
  let peek = function
      [] -> raise Empty
    | a :: rest -> a
end;;

module Queue 2 : QUEUE =
struct
  type 'a t = Queue of ('a list * 'a list)
  exception Empty
  let empty = Queue ([], [])
  let add q a =
    match q with
      Queue ([], _) -> Queue ([a], [])
    | Queue (fst, snd) -> Queue (fst, a :: snd)
  let take = function
      Queue ([], _) -> raise Empty 
    | Queue (a :: [], snd) ->
        (a, Queue (List.fold_left (fun x y -> y::x) [] snd, []))
    | Queue (a :: frest, snd) -> (a, Queue (frest, snd))
  let peek = function
      Queue ([], _) -> raise Empty
    | Queue (x :: _, _) -> x
end;;
