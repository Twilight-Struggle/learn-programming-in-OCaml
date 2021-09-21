(*6-13*)
type intseq = Cons of int * (int -> intseq);;
let rec fibcre x y = Cons(x+y, fibcre y);;
let fib = fibcre 1 0;;

let rec nthseq n (Cons(x, f)) = if n = 1 then x else nthseq (n-1) (f x);;

nthseq 10 fib;;

(*6-14*)
let is_prime x = 
  let rec is_divisible_from_2_to n =
    (n > 1) && ((x mod n = 0) || is_divisible_from_2_to (n-1)) in
  not (is_divisible_from_2_to (x-1));;
let rec next_prime x =
  if is_prime (x + 1) then x + 1 else next_prime (x + 1);;
let rec prime_seq x =
  if is_prime (x + 1) then Cons(x + 1, prime_seq) else prime_seq (x + 1);;

let is_prime1 x =
  let rec is_divisible i n =
    (i <= n) && ((x mod i = 0) || is_divisible (i+1) n) in
  not (is_divisible 2 (x-1));;
let rec prime_seq1 x =
  if is_prime1 (x + 1) then Cons(x + 1, prime_seq1) else prime_seq1 (x + 1);;

let is_prime2 x =
  let upper = int_of_float (floor (sqrt (float_of_int x))) in
  let rec is_divisible i n =
    (i <= n) && ((x mod i = 0) || is_divisible (i+1) n) in
  not (is_divisible 2 upper);;
let rec prime_seq2 x =
  if is_prime2 (x + 1) then Cons(x + 1, prime_seq2) else prime_seq2 (x + 1);;

let is_prime3 x prilist = 
  let rec is_divisible n = function
      [] -> x mod 2 = 0
    | a :: rest when a <= n -> (x mod a = 0) || (is_divisible n rest)
    | _ :: rest -> is_divisible n rest in
  not (is_divisible (x-1) prilist);;
let rec prime_seq3 primes x =
  if is_prime3 (x+1) primes then Cons(x+1, prime_seq3 ((x+1) :: primes))
  else prime_seq3 primes (x+1);;

let is_prime4 x prilist =
  let upper = int_of_float (floor (sqrt (float_of_int x))) in
  let rec is_divisible n = function
      [] -> x mod 2 = 0
    | a :: rest when a <= n -> (x mod a = 0) || (is_divisible n rest)
    | _ :: rest -> is_divisible n rest in
  not (is_divisible upper prilist);;
let rec prime_seq4 primes x =
  if is_prime4 (x+1) primes then Cons(x+1, prime_seq4 ((x+1) :: primes))
  else prime_seq4 primes (x+1);;

nthseq 3000 (prime_seq 1);;
nthseq 6000 (prime_seq1 1);;
nthseq 6000 (prime_seq2 1);;
nthseq 6000 (prime_seq3 [] 1);;
nthseq 6000 (prime_seq4 [] 1);; 

(*6-15*)
type ('a, 'b) sum = Left of 'a | Right of 'b;;
let f1 x s =
  match s with
    Left i -> Left (x, i)
  | Right i -> Right (x, i);;
let f2 s =
  match s with
    Left (a, b) -> a, Left b
  | Right (a, c) -> a, Right c;;
let f3 (s1, s2) =
  match (s1, s2) with
    (Left a, Left c) -> Left (Left (a, c))
  | (Right b, Left c) -> Left (Right (b, c))
  | (Left a, Right d) -> Right (Left (a, d))
  | (Right b, Right d) -> Right (Right (b, d));;
let f4 s =
  match s with
    Left (Left (a, c)) -> Left a, Left c
  | Left (Right (b, c)) -> Right b, Left c
  | Right (Left (a, d)) -> Left a, Right d
  | Right (Right (b, d)) -> Right b, Right d;;
let f5 (mes1, mes2) s =
  match s with
    Left a -> mes1 a
  | Right c -> mes2 c;;
let f6 x =
  let f a = x (Left a) in
  let g b = x (Right b) in
  (f, g);;
let f7 s a =
  match s with
    Left t1 -> Left (t1 a)
  | Right t2 -> Right (t2 a);; 
