type chiffre = int ;;(* entre 0 et 9*)

type chiffreCar = char ;;

type nombre = chiffreCar list ;;

type txtnb = char list ;;

let ccVc ( a : chiffreCar) : chiffre =
let b : int = int_of_char (a) in b-(int_of_char ('0')) ;;

let rec horner (a : int) (b : nombre) : int =
match b with
|[]->a
|e1::[] -> a*10 + ccVc e1
|e1::e2 -> let b3 = a*10 + ccVc e1 in horner (b3) (e2) ;;

let nbVnat ( a : nombre) : int =
match a with
|[]-> failwith("aucun nombre")
|e1::e2 -> horner (ccVc e1) (e2);;

let rec snbVsnat (a: nombre list) : int list =
match a with
|[] -> []
|e1::e2 -> (nbVnat e1)::snbVsnat(e2);;

let rec somme ( a : int list) : int =
match a with
|[] -> 0
|e1::e2 -> e1 + somme e2 ;;

let sup_esp( a : txtnb) : txtnb =
match a with
|' '::e2 -> e2
|e1 -> e1 ;;

let rec prnb_r (a:txtnb) : nombre*txtnb =
match a with
|[]->(a, [])
|' '::s -> ([], ' '::s)
|e1::s -> let (nb, reste) = prnb_r s in (e1::nb, reste) ;;


let rec les_nb (a:txtnb) : nombre list =
match a with
|[]->[]
|_ -> let (b1,b2) = prnb_r (sup_esp (a)) in b1::les_nb b2
;;

let somme_txtnb ( a: txtnb) : int =
somme (snbVsnat (les_nb a)) ;;



