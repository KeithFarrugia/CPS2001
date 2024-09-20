let rec factorial a = if a=1 then 1 else a * factorial (a - 1);;
let rec factorial a = match a with 1 -> 1 | _ -> a * factorial (a - 1);;

let isvowel c =  c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u';;
let isvowel c =  match c with 'a' -> true  | 'e' -> true  | 'i' -> true  | 'o' -> true  | 'u' -> true  | _ -> false;;

let rec gcd a b = if b=0 then a else gcd b (a mod b);;
let rec gcd a b = match b with 0 -> a | _ -> gcd b (a mod b);;

(*===================================QUESTIONS===================================*)

let not x =
  match x with
  | true -> false
  | false -> true;;

let rec sum x = 
  match x with
  | 1 -> 1
  | _ -> x + sum (x-1);;

let rec pow x n = 
  match n with
  | 0 -> 1
  | _ -> x * pow x (n-1);;


match 1+1 with 2 -> match 2+2 with 3 -> 4 | 4 -> 5

let isupper c =
  match c with
  | 'A'..'Z' -> true
  | _ -> false;;


let islower c =
  match c with
  | 'a'..'z' -> true
  | _ -> false;;