(*
Auteur : Guillaume GOBIN
Date : 24/02/2020
*)
open List
open Printf
open String

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
  printsn "";
  prints "buts : ";
  print_lln buts;
  prints "bf : ";
  prlistn bf;
  prints "backtrack : ";
  print_lln btk;;

let a, b, c, d, e, f, g, h = "a","b","c","d","e","f","g","h";;
let bf1 = [a;b];;
let br1 = [
            [a;b],h;
            [a;c],e;
            [a;h],e;
            [h;d],f;
            [h;a],f;
            
          ];;

  (* let br1 = function
    | c -> [[a;b]]
    | e -> [[a;d];[a;c]]
    | _ -> [[]]  *)
        
let but1 = [[f]];;

let rec apply_br br b = 
  match br with
    | [] -> []
    | (si, a)::suitebr -> if a = b then si::(apply_br suitebr b) else (apply_br suitebr b)

let is_a_fact bf b = List.exists (fun x -> x=b) bf;;

let top = function
  | [] -> failwith "liste vide"
  | a::q -> a

let pop = function
  | [] -> failwith "liste vide"
  | a::q -> q

let rec chainarr
  (************************************************************)
  (buts : string list list) (*la liste des conjonctions à prouver*)
  (br : (string list * string) list) (*la liste des règles*)
  (bf : string list) (*la liste des faits*)
  (backtrack : (string list) list)(*la liste des backtracks*)
  (proved : string list) (* liste des faits en attente d'être prouvé 
  avant d'être intégré à la base de fait *)
  (************************************************************)
  (* A partir d'une base de fait, et d'une base de règle, chainarr
  résoud un but en indiquant s'il y a une solution, les backtracks
  restant à explorer, et la base de fait mise à jour: 
  (aUneSolution, listeBacktrack, bfMaj)
  *)
  (************************************************************)
  : (bool * string list list * string list) =
  (************************************************************)
    print_etat_n buts backtrack bf;
    match buts with
    | [] -> (true, backtrack, bf)
    | conjonction :: suitebuts -> (
      match conjonction with
      | [] -> chainarr suitebuts br ((top proved)::bf) backtrack (pop proved)
      | b :: conjonction -> (
        if is_a_fact bf b then chainarr (conjonction::suitebuts) br bf backtrack proved
        else (
          let step = apply_br br b in
            match step with
            | [] -> ( match backtrack with
                      | [] -> (false, backtrack, bf)
                      | btk :: sbtk -> 
                        match conjonction with
                        | [] -> chainarr (btk::suitebuts) br bf sbtk proved
                        | c -> chainarr (btk::c::suitebuts) br bf sbtk proved)
            | si :: suitebtk ->
              match conjonction with
              | [] -> chainarr (si::suitebuts) br bf (suitebtk@backtrack) (b::proved)
              | c -> chainarr (si::c::suitebuts) br bf (suitebtk@backtrack) (b::proved)))) ;;

chainarr but1 br1 bf1 [] [];;
