(*=================REFERENCES===========*)

let l1 = ref 1;;
l1 := 2;;
let l2 = l1;;
!l2;;
l1:=3;;
!l2;;

let l3 = ref 1;;

let swap x y =
  let tmp = !x in 
  x :=!y;
  y :=tmp;;

if true then l1 := 0 else ();;
if true then l1 := 0;;

let p = 5;;
let q = 6;;
if p = q then 
  (l1 := !l1+1;
  l3 := !l3-1)
else
  l1:= !l1-1;;

if p = q then 
  begin
  l1 := !l1+1;
  l3 := !l3-1
  end
else
  l1:= !l1-1;;

for x = 1 to 5
  do
    print_int x; print_newline()
  done;;

let sum_ref x = 
  let acc = ref 0 in 
    while not (!x = 0) do
      acc := !acc + !x;
      x := !x -1;
    done;
    !acc;;

let list = ref [1;2;4;];;
!list;;

let smallest_pow2 x = 
  let t = ref 1 in 
  while !t < x do
    t := !t *2;
  done;
  !t;;

  let channel_statistics in_channel =
    let lines = ref 0 in
      try
        while true do
          let _ = input_line in_channel in
            lines := !lines + 1
        done
      with
        End_of_file ->
        print_string "There were ";
        print_int !lines;
        print_string " lines.";
        print_newline ();;

let file_statistics name =
  let channel = open_in name in
    try
      channel_statistics channel;
      close_in channel
    with
      _ -> close_in channel;;


let channel_statistics in_channel =
  let lines = ref 0 in
  let characters = ref 0 in
  let words = ref 0 in
  let sentences = ref 0 in
  try
    while true do
      let line = input_line in_channel in
        lines := !lines + 1;
        characters := !characters + String.length line;
        String.iter
          (fun c ->
          match c with
          '.' | '?' | '!' -> sentences := !sentences + 1
          | ' ' -> words := !words + 1
          | _ -> ())
        line
    done
  with
    End_of_file ->
    print_string "There were ";
    print_int !lines;
    print_string " lines, making up ";
    print_int !characters;
    print_string " characters with ";
    print_int !words;
    print_string " words in ";
    print_int !sentences;
    print_string " sentences.";
    print_newline ();;


let a1 = [|1; 2; 3; 4; 5|];;
a1.(0);;

a1.(1) <- 7;;
a1;;

a1.(5);;
a1.(5) <- 8;;

let a2 = Array.make 6 true;;

1.5;;
1.5+. 2.3;;

1.5 +. 6.;;

10. ** 2.;;
10. ** 100.2;;








(*ocamlc textstat.ml*)

(* does not work for some reason 
#load "langbasi.cmo";; 
Textstat.stats_from_file "gregor.txt"
*)


(*ocaml langbasi.mli landbasi.ml lang.ml -o stats*)
(*./stats gregor.txt*)