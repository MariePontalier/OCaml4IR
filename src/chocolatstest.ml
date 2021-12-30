open Chocolats 
open Gfile
open Tools
open Ford
open Printf

let () =

  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  and source = int_of_string Sys.argv.(2)
  and sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = chocolats_from_file infile in
  (*let graph = gmap graph int_of_string in*)
  let graph = graph_final graph (ford_fulkerson graph source sink) in
  let flot = flot_max graph source in
  let z = Printf.printf "%d chocolats pourront être mis en vente. \n Veuillez regarder le fichier choco.svg pour voir de quelle manière vous pouvez parvenir à ce résulat.\n " flot in
  let graph = gmap graph string_of_tuple in 


  (* Rewrite the graph that has been read. *)
  let () = write_file outfile graph in
  let () = export (outfile ^ ".dot") graph in


  ()

