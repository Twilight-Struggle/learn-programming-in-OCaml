(*14-3*)
let l1 = `Nil
and l2 = `Cons(1, `Nil)
and l3 = `Cons(2, `Cons(1, `Nil));;
let l6 = `Cons(1, `App(l2, l3));;

let make_max f = function
    `Cons(x, `Nil) -> x
  | `Cons(x, `Cons(y, l)) ->
      if x < y then f (`Cons(y, l)) else f (`Cons(x, l));;
let rec max l = make_max max l;;
max l3;;

let make_amax f = function
    (`Cons(_, (`Nil | `Cons(_, _)))) as cons -> make_max f cons 
  | `Cons(a, `App(l1, l2)) -> let x = f l1 in let y = f l2 in
      if x > y then (if a > x then a else x)
      else (if a > y then a else y)
  | `App(l1, l2) -> let x = f l1 in let y = f l2 in
      if x > y then x else y;;
let rec amax l = make_amax amax l;;
amax l6;;

let make_map self f = function
    `Nil -> `Nil
  | `Cons(a, l) -> `Cons(f a, self f l);;
let rec map f l = make_map map f l;;
map (fun x -> x * 2) l3;;

let make_amap self f = function
    (`Nil | `Cons(_, _)) as cons -> make_map self f cons
  | `App(l1, l2) -> `App(self f l1, self f l2);;
let rec amap f l = make_amap amap f l;;
amap (fun x -> x * 2) l6;;

(*14-4*)
(*
listは同じ型を前提としている。
同じコンストラクタが与えられればその中身を操作しようとする。
この時に別の型が含まれているとその操作が適用できない。
*)
