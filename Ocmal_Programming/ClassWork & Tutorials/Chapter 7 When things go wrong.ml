let rec drop n l =
  match l with
  | [] -> if n=0 then
           []
          else
            raise (Invalid_argument "drop")
  | h::t -> if n<0 then 
              raise (Invalid_argument "drop") 
            else if n=0 then
               l
            else drop (n - 1) t ;;