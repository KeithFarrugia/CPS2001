let times_ten x = x * 10;;

let both_zero a b =  if (a <> 0 && b <> 0) then true else  false ;;

let both_non_zero x y = x <> 0 && y <> 0;;

let rec sum x = if x = 1 then 1 else x + sum (x-1);;

let rec pow x  y = 
if (y = 0) then 
  1 
else 
  (x * pow x (y-1));;

let is_constant l = 
  l <> 'a' && l <> 'e' && l <> 'i' && l <> 'o' && l <> 'u';;
  
  
let rec factorial x =
if x <= 0 then 
   0 
else  if x = 1 then
   1 
else
    x * factorial (x - 1);;
    