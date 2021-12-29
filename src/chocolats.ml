open Ford
open Graph
open Printf

type cpath=string

(* Format d'un fichier 

   % définir une usine avec son id et sa production maximale
   usine 0 200

   % définir un magasin et son stockage maximal
   magasin 1 100

   % définir une route (arc) entre 2 noeuds avec le transport maximal
   route 0 1 150

*)


(*on utilise des id élevés pour pas mélanger avec les noeuds*)
let source = (1000)
let destination = (1001)

(*permet de lire une usine dans le fichier et de la relier au noeud source avec sa production max*)
let usine graph ligne =
  try Scanf.sscanf ligne "usine %d %d" (fun id production -> new_arc (new_node graph id) source id production)
  with e ->
    Printf.printf "Cannot read factory in line - %s:\n%s\n%!" (Printexc.to_string e) ligne ;
    failwith "from_file"
;;

(*permet de lire un magasin dans le fichier et de le relier au noeud destination avec son stockage max*)
let magasin graph ligne =
  try Scanf.sscanf ligne "magasin %d %d" (fun id stockage -> new_arc (new_node graph id) id destination stockage)
  with e ->
    Printf.printf "Cannot read store in line - %s:\n%s\n%!" (Printexc.to_string e) ligne ;
    failwith "from_file"
;;

(*permet de lire une route entre usine et magasin dans le fichier et de créer l'arc correspondant dans le graphe)
   peut-etre verifier que les noeuds existent bien ?*)
let route graph ligne =
  try Scanf.sscanf ligne "route %d %d %d"(fun id1 id2 transport -> new_arc graph id1 id2 transport)
  with e ->
    Printf.printf "Cannot read road in line - %s:\n%s\n%!" (Printexc.to_string e) ligne ;
    failwith "from_file"
;;

(*permet de lire un commentaire dans le fichier*)
let commentaire graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"

let chocolats_from_file cpath =
  let infile= open_in cpath in
  let graphe_initial = new_node (new_node empty_graph source) destination in
  let rec loop graph =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let graph2 =
        (* Ignore empty lines *)
        if line = "" then graph

        (* The first character of a line determines its content *)
        else match line.[0] with
          | 'u' -> usine graph line
          | 'm' -> magasin graph line
          | 'r' -> route graph line

          (* It should be a comment, otherwise we complain. *)
          | _ -> commentaire graph line
      in      
      loop graph2
    with End_of_file -> graph (* Done *)
  in

  let final_graph = loop graphe_initial in

  close_in infile ;
  final_graph