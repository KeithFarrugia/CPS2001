
let rec is_sorted l = 
  match l with
  | [] -> true
  | [a] -> true
  | h1::h2::t -> ((h1 <= h2) && (is_sorted (h2::t)));; (*Ascending*)


let rec insert x l =
  match l with 
  | [] -> [x]
  | h::t -> if (x <= h) then
              (x::l)
            else
              h::(insert x t);;

let rec insertion_sort l =
  match l with
  | [] -> []
  | h::t -> insert h (insertion_sort t);;

let rec merge l r = 
  match l, r with 
  | [], r -> r 
  | l,[] -> l
  | hl::tl, hr::tr -> if hl <= hr then
                       hl::(merge tl r)
                      else
                        hr::(merge l tr);;

let rec take n l =
  match l with
  | [] -> []
  | h::t -> if n <=0 then
              []
            else 
              h::(take (n-1) t);;

let rec take n l =
  match l with
  | [] -> []
  | h::t -> match n with 
             | 0 -> []
             | k -> h::(take (k-1) t);;
              
let rec drop n l =
  match l with
  | [] -> []
  | h::t -> if n <=0 then
              h::t
            else 
              take (n-1) t;;


let rec merge_sort l =
  let midpoint = (List.length l/2) in
    match l with 
    | []  -> []
    | [x] -> [x]
    | _   -> let left  = take midpoint l in 
             let right = drop midpoint l in
             merge (merge_sort left) (merge_sort right);;
let rec shitty_merge_sort l = 
  match l with
  | []->[]
  | h::t -> merge [h] (shitty_merge_sort t);;

let compute_on_4_2 f = f 4 2 
let plus x y = x-y;;
compute_on_4_2 plus;; (*this results in 6*)
compute_on_4_2 (fun x y -> x*y) (*result will be 8 this is a anonimous function no name*)

let rec double l = 
  match l with
  | [] -> []
  | h::t -> (h*2)::double t;;

let rec is_even l = 
  match l with 
  | [] -> []
  | h::t -> ((h mod 2) = 0) :: is_even t;;

let rec map f l = 
  match l with
  | []->[]
  | h::t -> (f h)::(map f t);;

let rec isort f l = (*added one parametter*)
  let rec insert x l = 
    match l with| [] -> [x]
    | h::t ->
      if (f x h)  (*changed this condition*)
        then x::h::t
     else h::insert x t
    in 
    match l with
    | [] -> []
    | h::t -> insert h (isort f t) ;; (*carry over parameter*)

isort (<=) [2;3;4;6;7;2;78;2;7];;
isort (>=) [2;3;4;6;7;2;78;2;7];;
(*===================================QUESTIONS===================================*)

let rec merge_sort l =
  let midpoint = (List.length l/2) in
    match l with 
    | []  -> []
    | [x] -> [x]
    | _   -> let left  = take midpoint l in 
             let right = drop midpoint l in
             merge (merge_sort left) (merge_sort right);;


let rec insert x l =
match l with 
| [] -> [x]
| h::t -> if (x >= h) then
            (x::l)
          else
            h::(insert x t);;

let rec insertion_sort l =
match l with
| [] -> []
| h::t -> insert h (insertion_sort t);; 
            
let rec is_sorted l = 
match l with
| [] -> true
| [a] -> true
| h1::h2::t -> ((h1 >= h2) && (is_sorted (h2::t)));; (*Descending*)

let rec sort l =
  let rec insert x s =
  match s with
  [] -> [x]
  | h::t ->
  if x <= h
  then x :: h :: t
  else h :: insert x t
  in
  match l with
  [] -> []
  | h::t -> insert h (sort t)