open Graph
open Gfile
open Tools
open Printf



type gpath = id list

let rec parcours_profondeur graph start stop marque = 
  if start=stop then [stop] 
  else 
    let arcs=out_arcs graph start in
    let rec next_arc = function
      | [] -> []
      |(i,l) :: rest -> if (List.mem i marque || l=0) then next_arc rest else let chemin = parcours_profondeur graph i stop (i::marque) in  
          if chemin=[] then next_arc rest else (start :: chemin)
    in next_arc arcs
;;


let rec min_label graph gpath min = match gpath with
  |[] -> min
  |x::y::rest ->  
    begin match (find_arc graph x y) with 
      |None -> 0
      |Some a ->  min_label graph (y::rest) (if  a<min then  a else min)
    end
  |x::rest -> min
;;

let rec changer_label graph gpath compteur = match gpath with 
  |[]-> graph
  |[x] -> graph
  |x::y::rest -> let graph1= add_arc graph x y (-compteur) in 
    changer_label (add_arc graph1 y x compteur) (y::rest) compteur 
;;

let graph_final graph_initial ff = 
  let arc id1 id2 = begin match (find_arc ff id1 id2) with 
    |None -> 0
    |Some a ->  a
  end in 

  let h gr id1 id2 cap = new_arc gr id1 id2 ((cap-(arc id1 id2), cap)) in
  let new_graph = (clone_nodes graph_initial) in 
  e_fold graph_initial h new_graph
;;

(*A REVOIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIR *)
let flot_max graph start = 
  let arcs = out_arcs graph start in
  let rec somme acu = function 
    |[] -> acu
    | (i,(l1,l2))::rest -> somme (acu+l1) rest
  in somme 0 arcs


let string_of_tuple (a,b) =
  "\""^ string_of_int a ^ "/" ^string_of_int b^"\"" ;;

let rec ford_fulkerson graph start stop =
  let chemin = parcours_profondeur graph start stop [start] in 
  (*let chemin_list = List.map string_of_int chemin in 
    let chemin_list = String.concat "," chemin_list in
    Printf.printf "Chemin trouvé : %s \n %!" chemin_list ;*)
  if chemin=[] then graph else 
    (let min = min_label graph chemin max_int in 
     (*printf "Min trouvé : %d \n %!" min ;*)
     let new_graph = changer_label graph chemin min in 
     ford_fulkerson new_graph start stop)
;;



