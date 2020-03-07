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
  | (x,y) :: q -> prints "{"; prlist x; prints "->"; prints y; _printbr q; prints "}"
  | [] -> prints "[]";;

let printetatn buts = 
  printsn "";
  printsn "buts : ";
  prlistn buts;;

let a, b, c, d, e, f, g, h = "a","b","c","d","e","f","g","h";;
let bf1 = [a;b];;
let br1 = [
            [a;b],h;
            [a;c],e;
            [a;d],e;
            [f;g],e;
            [a;d],f;
            [a;h],f;
            [a;b],g
          ];;

  (* let br1 = function
    | c -> [[a;b]]
    | e -> [[a;d];[a;c]]
    | _ -> [[]]  *)
        
let but1 = [e];;

let rec apply_br br b = 
  match br with
    | [] -> []
    | (si, a)::suitebr -> if a = b then si::(apply_br suitebr b) else (apply_br suitebr b)

let is_a_fact bf b = List.exists (fun x -> x=b) bf;;

let rec chainarr buts br bf nodes backtrack =
  printetatn buts;
  match buts with
  | [] -> (true, nodes, backtrack)
  | b :: suitebuts -> (
    if is_a_fact bf b then
      chainarr suitebuts br bf nodes backtrack
    else
      let step = apply_br br b in
        match step with
          | [] -> ( match backtrack with
                      | [] -> (false, nodes, backtrack)
                      | btk :: sbtk -> 
                        chainarr (btk@suitebuts) br bf nodes sbtk)
          | si :: suitebtk -> chainarr (si@suitebuts) br bf nodes suitebtk) ;;

chainarr but1 br1 bf1 [] [];;     