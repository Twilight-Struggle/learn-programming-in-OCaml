(*11-2*)
module type OrderedType = sig type t val compare : t -> t -> int end;;

module type SET =
sig
  type elt
  type t
  val empty : t (* 空集合 *)
  val is_empty : t -> bool (* 集合 か 空 かの テスト *)
  val mem : elt -> t -> bool (* elt が t に 属し て いる かの テスト *)
  val add : elt -> t -> t (* 要素 elt を t に 加え た 集合 を 返す *)
  val inter : t -> t -> t (* ふたつ の 集合 の 共通 部分 を 返す *)
  val union : t -> t -> t (* ふたつ の 集合 の 和集合 を 返す *)
  val diff : t -> t -> t (* ふたつ の 集合 の 差 を 返す *)
  val elements : t -> elt list (* 集合 要素 を 昇順 整列 済 リスト として 返す *)
end;;

module MakeAbstractSet (Order : OrderedType) : SET with type elt = Order.t =
struct
  type elt = Order.t
  type t = elt list
  let empty = []
  let is_empty set = if set = [] then true else false
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
  let rec union s1 s2 =
    match (s1, s2) with
      (s1, []) -> s1
    | ([], s2) -> s2
    | ((e1 :: rest1 as s1), (e2 :: rest2 as s2)) ->
        match Order.compare e1 e2 with
          0 -> e1 :: union rest1 rest2
        | r when r < 0 -> e1 :: union rest1 s2
        | _ -> e2 :: union s1 rest2
  let rec diff s1 s2 =
    match (s1, s2) with
      (s1, []) -> s1
    | ([], s2) -> s2
    | ((e1 :: rest1 as s1), (e2 :: rest2 as s2)) ->
        match Order.compare e1 e2 with
          0 -> diff rest1 rest2
        | r when r < 0 -> e1 :: diff rest1 s2
        | _ -> e2 :: diff s1 rest2
  let rec elements s = s end;;

module AbstractIntSet = MakeAbstractSet (
  struct
    type t = int
    let compare i j = i - j
  end);;

open AbstractIntSet;;
let s1 = add 1 (add 3 (add 4 (add 5 empty)));;
let s2 = add 2 (add 4 (add 6 (add 7 empty)));;
let emp = empty;;

is_empty emp;;
is_empty s1;;
elements (union s1 s2);;
elements (diff s1 s2);;
