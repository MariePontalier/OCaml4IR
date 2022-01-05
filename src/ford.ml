open Graph
open Gfile
open Tools
open Printf



type gpath = id list

(*Permet de parcourir un graphe en profondeur*)
let rec parcours_profondeur graph start stop marque = 
  if start=stop then [stop] 
  else (* si on n'est pas arrivés au puits*)
    let arcs=out_arcs graph start in (*liste des arcs sortants *)
    let rec next_arc = function
      | [] -> []
      |(i,l) :: rest -> if (List.mem i marque || l=0) then next_arc rest else let chemin = parcours_profondeur graph i stop (i::marque) in  
          if chemin=[] then next_arc rest else (start :: chemin) (* si au bout du chemin on n'a pas trouvé stop on revient au noeud i sinon on conserve le chemin *)
    in next_arc arcs
;;

(*Trouve le label ayant la valeur minimum dans un chemin donné*)
let rec min_label graph gpath min = match gpath with
  |[] -> min (* fin du parcours : on renvoie min *)
  |x::y::rest ->  
    begin match (find_arc graph x y) with 
      |None -> 0
      |Some a ->  min_label graph (y::rest) (if  a<min then  a else min)
    end
  |x::rest -> min (*il n'y a pas d'arc à explorer donc le minimum est celui trouvé précédemment*)
;;

(* Changer les labels des arcs *)
let rec changer_label graph gpath compteur = match gpath with 
  |[]-> graph
  |[x] -> graph
  |x::y::rest -> let graph1= add_arc graph x y (-compteur) in 
    changer_label (add_arc graph1 y x compteur) (y::rest) compteur (* suivant le sens de l'arc on ajoute ou soustrait la valeur compteur *)
;;

(*Permet d'obtenir le graphe avec le flot et la capacité *)
let graph_final graph_initial ff = 
  let arc id1 id2 = begin match (find_arc ff id1 id2) with (*permet de recuperer la valeur du label *)
    |None -> 0
    |Some a ->  a
  end in 
  let h gr id1 id2 cap = new_arc gr id1 id2 ((cap-(arc id1 id2), cap)) in (*crée l'arc avec le flot et la capcité *)
  let new_graph = (clone_nodes graph_initial) in (*graphe sans les arcs *)
  e_fold graph_initial h new_graph (*appliquer la fonction h au graph_initial dans le nouveau graphe *)
;;

(*Calcule le flot max du graphe : somme des labels des arcs sortants du noeud source*)
let flot_max graph start = 
  let arcs = out_arcs graph start in
  let rec somme acu = function 
    |[] -> acu
    | (i,(l1,l2))::rest -> somme (acu+l1) rest
  in somme 0 arcs

(* Permet de passer en string les tuples flot/capacité *)
let string_of_tuple (a,b) =
  "\""^ string_of_int a ^ "/" ^string_of_int b^"\"" ;;

(*Algo final*)
let rec ford_fulkerson graph start stop =
  let chemin = parcours_profondeur graph start stop [start] in (*on cherche un chemin entre le départ et l'arrivée*)
  (*let chemin_list = List.map string_of_int chemin in 
    let chemin_list = String.concat "," chemin_list in
    Printf.printf "Chemin trouvé : %s \n %!" chemin_list ;*)
  if chemin=[] then graph else (*si on ne trouve pas de chemin c'est que l'algo a fini de tourner *)
    (let min = min_label graph chemin max_int in (*on trouve le label min dans le chemin trouvé *)
     (*printf "Min trouvé : %d \n %!" min ;*)
     let new_graph = changer_label graph chemin min in (*on ajoute/enlève le min sur les arcs du chemin *)
     ford_fulkerson new_graph start stop)(*on recommence*)
;;



