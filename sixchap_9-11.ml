(*6-9*)
type token = PCDATA of string 
           | Open of string 
           | Close of string;;

type ('a, 'b) xml = XLf of 'b option | XBr of 'a * ('a, 'b) xml list;;

let tokens = [Open "a"; Open "b"; Close "b"; Open "c";
              PCDATA "Hello"; Close "c"; Close "a"];;

let xml_of_tokens token_list =
  let rec xml_of_tokens_in = function
      [] -> []
    |  (Open a) :: (Close _) :: rest -> XBr (a, [XLf None])
                                        :: (xml_of_tokens_in rest)
    | (Open a) :: rest -> [xml_of_tokens_toclose a [] rest]
    | (PCDATA a) :: rest -> XLf (Some a) :: (xml_of_tokens_in rest)
  and xml_of_tokens_toclose a cl = function
      (Close b) :: rest -> if a = b then XBr (a, xml_of_tokens_in cl)
        else xml_of_tokens_toclose a (cl @ [(Close b)]) rest
    | (_ as b) :: rest -> xml_of_tokens_toclose a (cl @ [b]) rest in
  let pop :: rest = xml_of_tokens_in token_list in pop;;

xml_of_tokens tokens;;

let rec string_of_xml = function 
    XBr (tag, xml_list) -> 
      "<" ^ tag ^ ">" ^ string_of_xmllist xml_list ^ "</" ^ tag ^ ">" 
  | XLf None -> "" 
  | XLf (Some s) -> s 
and string_of_xmllist = function
    [] -> "" 
  | xml :: rest -> string_of_xml xml ^ string_of_xmllist rest;;

string_of_xml (xml_of_tokens tokens);;

(*6-10*)
type arith = Const of int | Add of arith * arith | Mul of arith * arith;;

let rec eval = function
    Const a -> a
  | Add (b, c) -> (eval b) + (eval c)
  | Mul (a, b) -> (eval a) * (eval b);;

let exp = Mul (Add (Const 3, Const 4), Add (Const 2, Const 5));;

eval exp;;

(*6-11*)
let rec string_of_arith = function
    Const a -> string_of_int a
  | Add (a, b) -> "(" ^ string_of_arith a ^ "+" ^ string_of_arith b ^ ")"
  | Mul (a, b) -> "(" ^ string_of_arith a ^ "*" ^ string_of_arith b ^ ")";;

string_of_arith exp;;

let rec expand = function
    Mul (a, b) -> search_add a b
  | _ as el -> el
and search_add a b =
  match (a, b) with
    (Add (i, j), Add (k, l)) ->
      Add (Add (Mul (i, k), Mul (i, l)), Add (Mul (j, k), Mul (j, l)))
  | (i, j) -> Mul (expand i, expand j);;

string_of_arith (expand exp);;
