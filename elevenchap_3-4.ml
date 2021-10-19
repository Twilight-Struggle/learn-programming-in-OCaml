(*11-3*)
module type OrderedType = sig type t val compare : t -> t -> int end;;

module type SET =
sig
  type elt
  type t
  val empty : t (* 空集合 *)
  val mem : elt -> t -> bool (* elt が t に 属し て いる かの テスト *)
  val add : elt -> t -> t (* 要素 elt を t に 加え た 集合 を 返す *)
  val inter : t -> t -> t (* ふたつ の 集合 の 共通 部分 を 返す *)
  val elements : t -> elt list (* 集合 要素 を 昇順 整列 済 リスト として 返す *)
end;;

module MakeAbstractSet (Order : OrderedType) : SET =
struct
  type elt = Order.t
  type t = elt list
  let empty = []
  let rec mem elt = function 
      [] -> false
    | x :: rest ->
        let r = Order.compare elt x in
        (r = 0) || ((r > 0) && mem elt rest)
  let rec add elt = function
      [] -> [elt]
    | (x :: rest as s) ->
        match Order.compare elt x with
          0 -> s
        | r when r < 0 -> elt :: s
        | _ -> x :: (add elt rest)
  let rec inter s1 s2 =
    match (s1, s2) with
      (s1, []) -> []
    | ([], s2) -> []
    | ((e1:: rest1 as s1), (e2:: rest2 as s2)) ->
        match Order.compare e1 e2 with
          0 -> e1 :: inter rest1 rest2
        | r when r < 0 -> inter rest1 s2
        | _ -> inter s1 rest2
  let rec elements s = s end;;

module AbstractIntSet = MakeAbstractSet (
  struct
    type t = int
    let compare i j = i - j
  end);;

(*open AbstractIntSet;;
 let s1 = add 1 (add 3 (add 4 (add 5 empty)));;*)
(*AbstractIntSet.eltの型が解決できないため、
  intを入れてもOrder.tと一致するか確認できず動作しない*)

(*11-4*)
module Pair =
struct
  module Elt =
  struct
    type t = int
    let compare i j = i - j
  end
  module Set = MakeAbstractSet(Elt)
end;;

module type Psig =
sig
  module Elt : OrderedType
  module Set : SET with type elt = Elt.t
end;;

module MakeTest (P : Psig) =
struct
  let test_elements set =
    let rec loop = function
        []
      | [_] -> true
      | x:: y:: rest ->
          if P.Elt.compare x y > 0 then false else loop (y:: rest) in
    loop (P.Set.elements set)
end;;

module BadPair =
struct
  module Elt =
  struct
    type t = int
    let compare i j = i - j
  end
  module Set = MakeAbstractSet(
    struct
      type t = string (* 文字列 の 集合 に なっ て しまう! *)
      let compare s1 s2 = 0
    end)
end;;

module Test = MakeTest (BadPair);;
