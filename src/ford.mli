open Graph
open Gfile
open Tools
open Printf

type gpath = id list
val parcours_profondeur : int graph -> id -> id -> gpath -> gpath;;
val min_label : int graph -> gpath -> int -> int
val changer_label : int graph -> gpath -> int -> int graph
val ford_fulkerson : int graph -> id -> id -> int graph
