(*6-1*)
type figure =
    Point
  | Circle of int
  | Rectangle of int * int
  | Square of int;;

let similar x y =
  match (x, y) with
    (Point, Point) | (Circle _, Circle _) | (Square _, Square _) -> true
  | (Rectangle (l1, l2), Rectangle (l3, l4)) -> (l3 * l2 - l4 * l1) = 0
  | (Rectangle (l1, l2), Square _) | (Square _, Rectangle (l1, l2)) -> l1 = l2
  | _ -> false;;

similar (Rectangle (2, 4)) (Rectangle (1, 2));;
similar (Square 2) (Rectangle (5, 5));;

(*6-2*)
type figure_with_loc =
    Point of int * int
  | Rectangle of  int * int * int * int
  | Square of int * int * int;;

let overlap x y =
  match (x, y) with
    (Point (p1, p2), Point(p3, p4)) -> false
  | ((Point (p1, p2), Square(axis, p3, p4)) |
     (Square(axis, p3, p4), Point (p1, p2)))  ->
      (p3 - axis/2 < p1 && p1 < p3 + axis/2) &&
      (p4 - axis/2 < p2 && p2 < p4 + axis/2)
  | ((Point (p1, p2), Rectangle(l1, l2, p3, p4)) |
     (Rectangle(l1, l2, p3, p4), Point (p1, p2)))  ->
      (p3 - l1/2 < p1 && p1 < p3 + l1/2) &&
      (p4 - l2/2 < p2 && p2 < p4 + l2/2);;

overlap (Point (2, 4)) (Rectangle (4, 4, 1, 3));;