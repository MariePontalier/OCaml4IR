(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes gr = 
  n_fold gr new_node empty_graph
;;

let gmap gr f = 
  let h acu id1 id2 label = new_arc acu id1 id2 (f label) in
  e_fold gr h (clone_nodes gr) ;;


let add_arc gr id1 id2 n= match (find_arc gr id1 id2) with 
  | None -> (new_arc gr id1 id2 n) 
  | Some x -> new_arc gr id1 id2 (x+n) ;;
