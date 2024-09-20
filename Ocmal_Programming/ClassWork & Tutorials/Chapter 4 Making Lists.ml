let isnil l =
  match l with
  | [] -> true
  | _ -> false;;

let rec length l =
  match l with
  | [] -> 0
  | h::t -> 1 + length t;;
let rec length l =
  match l with
  | [] -> 0
  | _::t -> 1 + length t;;

let rec sum l =
  match l with
  | [] -> 0 
  | h::t -> h + sum t

let rec length_inner l n =
  match l with
  | [] -> n
  | h::t -> length_inner t (n + 1) 
let length l = length_inner l 0

let rec odd_elements l =
  match l with
  | [] -> []
  | [a] -> [a]
  | a::_::t -> a :: odd_elements t

let rec odd_elements l =
  match l with
  | a::_::t -> a :: odd_elements t
  | _ -> l

let rec append a b =
 match a with
  | [] -> b
  | h::t -> h :: append t b

let rec rev l =
  match l with
  [] -> []
  | h::t -> rev t @ [h]

let rec take n l =
  if n=0 then 
    []
 else
  match l with
  |h::t -> h :: take (n - 1) l
  |_ -> [];;

let rec drop n l =
  if n=0 then
     l
  else
    match l with
    | h::t -> drop (n - 1) t
    |_ -> [];;

(*===================================QUESTIONS===================================*)

let rec odd_elements l =
  match l with
  | _::b::t -> b :: odd_elements t
  | _ -> l;;

let rec count_true l = 
  match l with
  | true::t  -> 1 + count_true t
  | false::t -> count_true t
  | _ -> 0;;
let rec count_true c l = 
  match l with
  | true::t  -> count_true (c+1) t
  | false::t -> count_true c t
  | _ -> c;;

let rec palindrome l = 
  match l with
  | h::t -> [h] @ (palindrome t) @ [h]
  | _ -> [];;

let mk_palindrome l =
  l @ rev l;;
let is_palind l =
  l = rev l;;

let rec drop_last l =
  match l with
  | [] -> []
  | h::[] -> []
  | h::t -> h::drop_last t;;
let rec drop_last l =
  match l with
  | [] -> []
  | [_] -> []
  | h::t -> h :: drop_last t;;
let rec drop_last_inner a l =
  match l with
  | [] -> rev a
  | [_] -> rev a
  | h::t -> drop_last_inner (h :: a) t;;

let rec member e l = 
  match l with 
  | [] -> false
  | h::t -> if h = e then true else member e t;;  
let rec member e l =
  match l with
  |[] -> false
  | h::t -> h = e || member e t ;;

let rec make_set l =
  match l with
  [] -> []
  | h::t -> if member h t then make_set t else h :: make_set t

let rec make_set l acc =
  match l with
  | [] -> acc
  | h::t -> if member h acc then
               make_set t acc
            else
               make_set t (acc @  [h]);;


let rec rev_inner a l =
  match l with
  | [] -> a
  | h::t -> rev_inner (h :: a) t;;