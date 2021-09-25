(*8-1*) 
type 'a ref = { mutable contents : 'a };;
(* ref *)
let p = { contents = 5 };;
(* ! *)
p.contents;;
(* := *)
p.contents <- 3;;

(*8-2*)
let incr x =
  x:= !x + 1;;

let x = ref 3;;
incr x;;
!x;;

(*8-3*)
(* 参照を使って無理やり再帰している*)

(*8-4*)
let fib n =
  let bef = ref 0 and aft = ref 1 and i = ref 1 in
  while (!i <= n) do
    begin
      if !i = 1 then aft := 1
      else let temp = !bef + !aft in bef := !aft; aft := temp
    end;
    i := !i + 1
  done;
  !aft;;

fib 10;;

(*8-5*)
(* let x = ref []の時点では型は決まっていないが、`aの代わりに`_weak1で示される
_weak1型では何かを入れた瞬間型が決まり変えられなくなる *)

(*8-6*)
let forr i n body =
  let ri = ref i and rn = ref n in
  let rec forr_in ri rn =
    if !ri <= !rn then 
      begin
        body();
        ri := !ri + 1;
        forr_in ri rn 
      end in
  forr_in ri rn;;

forr 1 3 (fun () -> print_string "test\n");;
