(*
Auteur : Guillaume GOBIN
Date : 24/02/2020
*)

#use "_print.ml" ;;
        
let rec apply_br br b  = 
  match br with
    | [] -> []
    | (si, a)::suitebr -> if a = b then si::(apply_br suitebr b) else (apply_br suitebr b);;

let is_a_fact bf b =
  List.exists (fun x -> x=b) bf;;

let top = function
  | [] -> failwith "liste vide"
  | a::q -> a

let pop = function
  | [] -> failwith "liste vide"
  | a::q -> q

let rec chainarr buts br bf backtrack proved =
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
      (printsn "Résolu !"; prints "L'asteroide détecté est "; printsn (top (top buts));)
    else 
      (printsn "Non résolu !"; prints "L'asteroide détecté n'est pas "; printsn (top (top buts)););;

