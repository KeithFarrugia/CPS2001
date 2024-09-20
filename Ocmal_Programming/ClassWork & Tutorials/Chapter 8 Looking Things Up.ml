let grade = ("alice", 'A');;

let snd p =  match p with (_,gr) -> gr;;

let fst (k,_) = k;;

fst grade;;

let grades = [grade; ("Bob",'B'); ("Chad", 'F') ];;
let rec lookup x l =
  match l with
  | [] -> raise Not_found
  | (key, value)::t-> if key =x then value else lookup x t;;

let rec update k v d = 
  match d with
  | [] -> [(k,v)]
  | (k',v')::t -> if k' = k then (k,v)::t else (k',v')::update k v t;;

let rec remove k m =
  match m with
  | [] -> []
  | (k',v')::t -> if(k' = k) then t else (k',v')::remove k t;;

let keyexists k m =
  try
    let _ = lookup k m in true
  with
    Not_found -> false;;

let add x y = x+y;;
let addsix = add 6;;

( + );;
( + ) 2;;
let addtwo = ( + )2;;

let rec map f l = match l with
|[] ->[] 
| h::t -> (f h)::(map f t);;
map (( + ) 2) [1;2;3];;

let lol =  [[];[2;4]; [5;3;1;]];;

let rec mapl f l =
  match l with 
  | [] ->[]
  | h::t -> (map f h)::(mapl f t);;

let maplst f l = map (map f) l;;

