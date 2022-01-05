(* Yes, we have to repeat open Graph. *)
open Graph

(*retourne un nouveau graphe avec les mêmes noeuds mais sans les arcs *)
let clone_nodes gr = 
  n_fold gr new_node empty_graph
;;

(*Permet d'appliquer la fonction f à tous les arcs du graph *)
let gmap gr f = 
  let h acu id1 id2 label = new_arc acu id1 id2 (f label) in
  e_fold gr h (clone_nodes gr) ;;

(*Si l'arc existe déjà on ajoute n à son label sinon on crée un arc avec n comme label *)
let add_arc gr id1 id2 n= match (find_arc gr id1 id2) with 
  | None -> (new_arc gr id1 id2 n) 
  | Some x -> new_arc gr id1 id2 (x+n) ;;
