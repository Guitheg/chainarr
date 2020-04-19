(*
Auteur : Guillaume GOBIN
Date : 24/02/2020
*)

#use "_print.ml" ;;
        
let rec apply_br 
  (************************************************************)
  (br : (string list * string) list) (*la liste des règles*)
  (b : string) (*le but en cours*)
  : (string list list) = 
  (* On applique ici la base de règle, on infère*)
  (************************************************************)
  match br with
    | [] -> []
    | (si, a)::suitebr -> if a = b then si::(apply_br suitebr b) else (apply_br suitebr b);;

let is_a_fact 
(bf : string list) (*la liste des faits*)
(b : string) (*le but en cours*)
: (bool) = 
(* On vérifie si un but est dans la base de fait*)
List.exists (fun x -> x=b) bf;;

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
              
let resoudre buts br bf backtrack proved = 
  let (res, _, _) = chainarr buts br bf backtrack proved in
    if res = true then
      match buts with
      (a :: q) :: l -> (printsn "Résolu !"; prints "L'asteroide détecté est "; printsn a)
    else (printsn "Non résolu !"; prints "L'asteroide détecté n'est pas "; printsn a);;
              