open Graph
open Gfile
open Tools
open Printf

type gpath = id list
val parcours_profondeur : int graph -> id -> id -> gpath -> gpath;;
val min_label : int graph -> gpath -> int -> int
val changer_label : int graph -> gpath -> int -> int graph
val graph_final : int graph -> int graph -> (int*int) graph
val ford_fulkerson : int graph -> id -> id -> int graph
val string_of_tuple : (int*int) -> string
val flot_max : (int*int) graph -> id -> int
