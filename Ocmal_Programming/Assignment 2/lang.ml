type alpha = A | B;;
type astring = alpha list;;
type alang = astring list;;
exception Not_in_alphabet;;


(* A few test strings *)

let test1: astring = [A; B; B; A; A ;B ];;
let test2: astring = [B; B; A; B];;
let test3: astring = [A; B; A; B];;
let test4: astring = [];;
let test5: astring = [B; B; B];;

(* Generic higher-order functions *)

let rec map (f: 'a -> 'b)  (s: 'a list): 'b list =
  match s with 
  | []   -> []
  | h::t -> (f h) :: (map f t);;

let rec reduce (f: 'a -> 'b -> 'a) (acc : 'a) (s: 'b list): 'a = 
  match s with
  | []   -> acc
  | b::t -> reduce f (f acc b) t;;



(* Alphabet letter equality  *)

let a_eq (a: alpha) (b: alpha): bool =
(* ---------------- edit here begin ---------------- *)
  a = b;;
(* ----------------- edit here end ----------------- *)



(* Functions over strings  *)

let rec len (s : astring): int =
  match s with 
  | []      -> 0
  | _ :: t  -> 1 + (len t);;


let rec has_a (a : alpha) (s : astring): bool =
   match s with 
   | []     -> false
   | b :: t -> if (a_eq a b) then true else has_a a t ;;
  

let rec flt_a (a : alpha) (s : astring): astring =
   match s with 
   | []     -> []
   | b :: t -> if (a_eq a b) then (flt_a a t) else b :: (flt_a a t) ;;


   
let rec 
  even_a (a : alpha) (s : astring): bool =
    (* ---------------- edit here begin ---------------- *)
    match s with 
    | [] -> true
    | h::t -> if (h=a) then
                odd_a  a t
              else
                even_a a t
    (* ----------------- edit here end ----------------- *)
and 
  odd_a (a : alpha) (s : astring): bool =
    (* ---------------- edit here begin ---------------- *)
    match s with 
    | [] -> false
    | h::t -> if (h=a) then
                even_a a t
              else
                odd_a a t;;
    (* ----------------- edit here end ----------------- *)

    

(* String predicates *)

let has_A :  astring -> bool = 
(* ---------------- edit here begin ---------------- *)
    has_a A;;
(* ----------------- edit here end ----------------- *)

let has_B :  astring -> bool = 
(* ---------------- edit here begin ---------------- *)
    has_a B;; (* dummy code  *)
(* ----------------- edit here end ----------------- *)

let has_odd_B : astring -> bool = 
(* ---------------- edit here begin ---------------- *)
    odd_a B;; (* dummy code  *)
(* ----------------- edit here end ----------------- *)

let rec is_ABstar (s : astring): bool =
(* ---------------- edit here begin ---------------- *)
  match s with
  | [] -> true
  | A::B::t -> (is_ABstar t)
  | _::t -> false;;
(* ----------------- edit here end ----------------- *)

let is_ABplus (s : astring): bool = 
(* ---------------- edit here begin ---------------- *)
match s with
| [] -> false
| _::t -> is_ABstar s;;
(* ----------------- edit here end ----------------- *)




(* Function redefinitions using reduce() *)

let len_red : astring -> int = 
(* ---------------- edit here begin ---------------- *)
    let inc acc _ = acc+1 in
      reduce (inc) 0  ;; (* dummy code  *)
(* ----------------- edit here end ----------------- *)
  

let has_A_red : astring -> bool = 
(* ---------------- edit here begin ---------------- *)
  let has (h : bool) (a : alpha) = h || (a = A) in                                 
      reduce has false ;;
(* ----------------- edit here end ----------------- *)




(* sets and membership *)

let is_subset (l: alang) (p: astring -> bool): bool = 
(* ---------------- edit here begin ---------------- *)
  let check (b : bool) (s : astring) =  (b && p s) in
      reduce check true l ;; (* dummy code  *)
(* ----------------- edit here end ----------------- *)

