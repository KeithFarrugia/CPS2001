(*========================== QUESTION 1 ==========================*)
let is_even x =  x mod 2 = 0;;

(*========================== QUESTION 2 ==========================*)
let rec filter_int l x = 
 match l with 
 | []->[] 
 | h::t ->
  if h = x then
    filter_int t x
  else
    h::(filter_int t x);;

(*========================== QUESTION 3 ==========================*)
let rec sum_in c l = 
  match l with
  | h::t -> sum_in (c+h) t 
  | _    -> c;;

let sum l = sum_in 0 l;;

(*========================== QUESTION 4 ==========================*)

let rec reverse_in l s = 
  match l with 
  | h::t -> reverse_in t ([h]@s)
  | _ -> s;;

let reverse l = reverse_in l [];;

(*========================== QUESTION 5 ==========================*)

let rec palindrome l =
  l @ reverse l;;

(*========================== QUESTION 6 ==========================*)

let rec palindrome l =
  l = reverse l;;