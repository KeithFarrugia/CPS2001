(* Type Declaration *)

type alpha = char;;
exception Not_in_alphabet of alpha;;

(* Utility Functions *)

let explode_string (s : string) : alpha list = List.init (String.length s) (String.get s);;
let char_to_string (c: char): string = Stdlib.Char.escaped c;;


(* Pre-declared strings for testing pruposes *)

let test1 = explode_string "abbaab";;
let test2 = explode_string "bbab";;
let test3 = explode_string "bca";; (* contains a character that is not in Sigma = {a,b} *) 

(* Basic operations *)


(* Question 1 *)

let rec num_of_c (c : alpha) (s : alpha list): int =
(* ---------------- edit here begin ---------------- *)
  match s with
  | [] -> 0
  | h::t -> if (h = c) then
              1+num_of_c c t
            else
              num_of_c c t;;
(* ----------------- edit here end ----------------- *)


(* Question 2 *)

let rec has_c (c : alpha) (s : alpha list): bool =
(* ---------------- edit here begin ---------------- *)
  match s with
  | [] -> false
  | h::t -> if (h = c) then
              true
            else
              has_c c t;;
(* ----------------- edit here end ----------------- *)


(* Question 3 *)

let rec flt_c (c : alpha) (s : alpha list): alpha list =
(* ---------------- edit here begin ---------------- *)
  match s with
  | [] -> []
  | h::t -> if (h = c) then
              flt_c c t
            else
              h::flt_c c t;;
(* ----------------- edit here end ----------------- *)



(* Question 4 *)

let rec invert (s : alpha list): alpha list =
(* ---------------- edit here begin ---------------- *)
match s with
| [] -> []
| h::t -> if (h = 'a') then
            ['b']@invert t
          else if (h = 'b') then
            ['a']@invert t
          else
            raise (Not_in_alphabet h);;
(* ----------------- edit here end ----------------- *)



(* Question 5 *)

let rec len_acc  (s : alpha list) (n : int): int =
(* ---------------- edit here begin ---------------- *)
  match s with
  | [] -> n
  | h::t -> len_acc t (n+1);;
(* ----------------- edit here end ----------------- *)

let len (s : alpha list): int = len_acc s 0;;

(* #trace len;; *)



(* Question 6 *)

let rec map f l =
  match l with 
  | []   -> []
  | h::t -> (f h) :: (map f t);;

(* ---------------- edit here begin ---------------- *)
let invert_func (s : alpha): alpha = 
  match s with
  | 'a' -> 'b'
  | 'b' -> 'a'
  | _ -> raise (Not_in_alphabet s);;
(* ----------------- edit here end ----------------- *) 

let rec inv_map (s : alpha list): alpha list =  
(* ---------------- edit here begin ---------------- *)
  map invert_func s;;
(* ----------------- edit here end ----------------- *)  




(* Question 7 *)

(* ---------------- edit here begin ---------------- *)
let rec concat_inner (s : alpha list) (t : alpha list) (acc : alpha list): alpha list =
  match s,t with
  | []    , []     -> acc
  | []    , ht::tt -> concat_inner s tt (ht::acc)
  | sh::st, []     -> concat_inner st t (sh::acc)
  | sh::st, ht::tt -> concat_inner st t (sh::acc);;



let rec reverse (s : alpha list) (a : alpha list): alpha list =
  match s with
  | [] -> a
  | h::t -> reverse t (h::a);;
(*  add any axillary functions used by concat  here  *)
(* ----------------- edit here end ----------------- *) 

let rec concat (s : alpha list) (t : alpha list): alpha list =
(* ---------------- edit here begin ---------------- *)
  reverse (concat_inner s t []) ([]);;
  
(* ----------------- edit here end ----------------- *) 

