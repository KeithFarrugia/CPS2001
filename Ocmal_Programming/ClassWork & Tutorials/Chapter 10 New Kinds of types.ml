type colour = 
| Red 
| Green 
| Blue
| RGB of int * int * int;;

let yellow = RGB (0, 255, 255);;

let col_to_rgb c = 
match c with
| Red -> (255, 0, 0)
| Blue -> (0, 0, 255)
| Green -> (0, 255, 0)
| RGB(r,g,b)-> (r,g,b);;

type 'a option = 
| None
| Some of 'a;;

Some 5;;
Some true;;

let rec lookup_opt x l =
  match l with
  | [] -> None
  | (k, v)::t -> if x=k then Some v else lookup_opt x t;;

let key_exists_opt k d =
  match lookup_opt k d with 
  | None -> false 
  | Some _ -> true;;

type 'a sequence = 
| Nil
| Cons of 'a * 'a sequence

let rec lenn s =
  match s with 
  | Nil -> 0;
  | Cons(_, s') -> (1 + lenn s');;

type expr = 
| Num of int
| Add of expr * expr
| Sub of expr * expr;;

let rec eval e = 
  match e with
  | Num x -> x
  | Add (e, e') -> eval e + eval e'
  | Sub (e, e') -> eval e - eval e';;