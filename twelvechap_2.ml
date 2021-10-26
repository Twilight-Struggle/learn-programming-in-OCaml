(*12-2*)
class calc =
  object
    val mutable num = 0
    val mutable inter = 0
    val mutable func = fun x -> x
    method input n = num <- n
    method plus = let x = num + inter in func <- (fun y -> x + y); inter <- x
    method eq = inter <- 0; func num
  end;;

let c = new calc;;
c#input 4; c#plus; c#input 2; c#plus; c#input 3; c#plus; c#input 5; c#eq;;
