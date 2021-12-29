open Chocolats 
open Gfile
open Tools
open Ford

let () =
  if Array.length Sys.argv <> 3 then
    begin
      Printf.printf "\nUsage: %s infile outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(2)
  in

  (* Open file *)
  let graph = from_file infile in
  let graph = gmap graph int_of_string in
  let graph = graph_final graph (ford_fulkerson graph source sink) in
  let flot = flot_max graph source in
  let z = Printf.printf "Le flot maximal est : %d \n" flot in
  let graph = gmap graph string_of_tuple in 


  (* Rewrite the graph that has been read. *)
  let () = write_file outfile graph in
  let () = export (outfile ^ ".dot") graph in


  ()

