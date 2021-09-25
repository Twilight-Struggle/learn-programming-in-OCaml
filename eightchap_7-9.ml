(*8-7*) 
let array_iter f array =
  try
    (for i = 0 to 100000 do
       f array.(i)
     done)
  with Invalid_argument "index out of bounds" -> ();;

array_iter (fun s -> print_string "Station: "; print_endline s)
  [|"Tokyo"; "Shinagawa"; "Shin-Yokohama"; "Nagoya"; "Kyoto"; "Shin-Osaka"|];;

(*8-8*)
let array_iteri f array =
  try
    (for i = 0 to 100000 do
       f (i+1) array.(i)
     done)
  with Invalid_argument "index out of bounds" -> ();;

array_iteri (fun i s -> print_string "Station #";
              print_int i; print_string ": "; print_endline s)
  [|"Tokyo"; "Shinagawa"; "Shin-Yokohama"; "Nagoya"; "Kyoto"; "Shin-Osaka"|];;

(*8-9*)
type 'a mlist = MNil | MCons of 'a * 'a mlist ref;;
type 'a queue = {mutable head : 'a mlist; mutable tail : 'a mlist};;
let create () = {head = MNil; tail = MNil};;
let q : int queue = create();;
let add a = function
    {head = MNil; tail = MNil} as q ->
      let c = MCons(a, ref MNil) in q.head <- c; q.tail <- c
  | {tail = MCons(_, next)} as q ->
      let c = MCons(a, ref MNil) in next := c; q.tail <- c
  | _ -> failwith "enqueue: input queue broken";;
let peek = function
    {head = MNil; tail = MNil} -> failwith "hd: queue is empty"
  | {head = MCons( a, _)} -> a
  | _ -> failwith "hd: queue is broken";;
let take = function
    {head = MNil; tail = MNil} -> failwith "dequeue: queue is empty"
  | {head = MCons(a, next)} as q -> q.head <- !next; a
  | _ -> failwith "dequeue: queue is broken";;

let dequeue = function
    {head = MNil; tail = MNil} -> failwith "dequeue: queue is empty"
  | {head = MCons(a, next)} as q ->
      q.head <- !next; if !next = MNil then q.tail <- MNil; a
  | _ -> failwith "dequeue: queue is broken";;

add 1 q; add 2 q; add 3 q;;
dequeue q; dequeue q; add 4 q; dequeue q; dequeue q; add 5 q; peek q;;
