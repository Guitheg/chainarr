open List ;;
open Printf ;;
open String ;;

let printsn str = Printf.printf "%s\n" str;; 
let prints str = Printf.printf "%s" str;;
let printin i = Printf.printf "%d\n" i;;
let printi i = Printf.printf "%d" i;;

let rec _print_list = function
  | x :: q -> prints ", "; prints x; _print_list q;
  | [] -> prints "";;
let prlist = function 
  | x :: q -> prints "["; prints x; _print_list q; prints "]"
  | [] -> prints "[]";;
let prlistn = function 
  | x :: q -> prints "["; prints x; _print_list q; prints "]\n"
  | [] -> prints "[]\n";;
let rec _printbr = function
  | (x,y) :: q -> prints ", "; prlist x; prints "->"; prints y; _printbr q;
  | [] -> prints "";;
let printbrn = function
  | (x,y) :: q -> prints "{"; prlist x; prints "->"; prints y; _printbr q; prints "}\n"
  | [] -> prints "[]\n";;
let printbr = function
  | (x,y) :: q -> prints "{"; prlist x; prints "->"; prints y; _printbr q; prints "}";
  | [] -> prints "[]";;

let rec _print_ll = function
  | c :: suitebuts -> prints ", "; prlist c; _print_ll suitebuts;
  | [] -> prints "";;

let print_lln = function
  | c :: suitebuts -> prints "{"; prlist c; _print_ll suitebuts; prints "}\n";
  | [] -> prints "{}\n";;
  
let print_etat_n buts btk bf = 
  printsn "*************";
  prints "La liste des faits à montrer est : ";
  print_lln buts;
  printsn "";
  prints "Les faits déduit sont : ";
  prlistn bf;
  printsn "";
  prints "Nous pouvons encore inférer sur : ";
  print_lln btk;
  printsn "*************";
