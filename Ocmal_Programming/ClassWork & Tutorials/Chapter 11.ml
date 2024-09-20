type 'a tree = 
| BR of 'a * 'a tree * 'a tree
| LF;;

let rec size tr =
  match tr with
  | LF -> 0
  | BR (_,l,r) -> 1 + size l + size r;;

  let rec total tr =
    match tr with
    | LF -> 0
    | BR (n,l,r) -> n + total l + total r;;


let rec max_depth tr =
  match tr with
  | LF -> 0
  | BR (_,l,r)-> 1+ max(max_depth l) (max_depth r);; 


let rec min_depth tr =
  match tr with
  | LF -> 0
  | BR (_,l,r)-> 1+ min(min_depth l) (min_depth r);; 

  let rec max_elem tr =
    match tr with
    | LF -> 0
    | BR (x,l,r)-> 
      let ml = max_elem l in 
        let mr = max_elem r in 
          max x (max ml mr);; 
        
let rec min_elem tr = 
  match tr with
  |LF -> 1000
  |BR (x,l,r) -> min x (min (min_elem l) (min_elem r));;

let rec tree_mem x tr = 
  match tr with 
  | LF -> false 
  | BR (y,l,r) -> ((x = y)|| (tree_mem x l) || (tree_mem x r));;


let rec flatten tr = 
  match tr with
  |LF -> []
  |BR (x,l,r) -> (flatten l) @ [x] @ (flatten r);;

let rec tree_map f tr = 
  match tr with
  |LF -> LF
  |BR (x,l,r) -> BR(f x, tree_map f l, tree_map f r);;


type bst = int tree;;

let rec is_bst tr =
  match tr with
  |LF -> true
  |BR (x,l,r) -> ((max_elem l) <= x) && (x <= (min_elem r))
                  && (is_bst l) && (is_bst r);;


let rec insert_bst x tr =
  match tr with
  | LF -> BR(x, LF, LF)
  | BR (y, l, r) -> if (x = y) then BR(y, l, r) (* dont inser duplicates  *)
                    else if (x < y) then BR (y , (insert_bst x l), r)
                    else BR (y, l, (insert_bst x r));;

let rec tree_mem_bst x tr = 
  match tr with 
  | LF -> false 
  | BR (y,l,r) -> if (x = y) then true
                  else if (tree_mem x l) then true
                  else(tree_mem x r);;

print_int 100;;

let f = fun x -> print_int x;;

f 6;;

print_int 1; print_newline(); print_int 2; print_newline();;
();;

let dict = [(1,"one"); (2, "two"); (3, "three")];;
let print_dict_entry (k, v) = 
  print_int k;
  print_newline ();
  print_string v;
  print_newline();;

let rec print_dict d =
  match d with
  | [] -> ()
  | h::t -> print_dict_entry h; print_dict t;;


let rec iter f l =
  match l with
  |[] -> ()
  | h::t -> f h; iter f t ;;


let print_dict_iter = iter print_dict_entry;;

let rec read_dict () =
  try 
    let i = read_int () in
      if i = 0 then 
        [] 
      else
        let name = read_line() in
        (i, name)::read_dict ()
  with 
    Failure _ -> (*ignore arguement "int_of_string"*)
    print_string "This is not a valid integer, Please try again";
    print_newline (); read_dict();;

let entry_to_channel ch (k,v)=
  output_string ch (string_of_int k);
  output_char ch '\n';
  output_string  ch v;
  output_char ch '\n';;

let dictionary_to_channel ch dict =
  iter (entry_to_channel ch) dict;;

let dictionary_to_file filename dict = 
  let ch = open_out filename in
  dictionary_to_channel ch dict; close_out ch;;

let entry_of_channel ch = 
  let number = input_line ch in 
    let name = input_line ch in 
      (int_of_string number, name);;

let rec dictionary_of_channel ch =
  try
    let e = entry_of_channel ch in 
    e:: dictionary_of_channel ch
  with
    End_of_file -> [];;

let dictionary_of_file filename = 
  let ch = open_in filename in
   let dict = dictionary_of_channel ch in
    close_in ch; dict;;


