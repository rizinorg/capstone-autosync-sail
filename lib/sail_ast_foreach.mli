open Libsail
open Ast
open Ast_defs
open Sail_ast_processor

val foreach_node :
  ('a, 'b) ast -> ('a, 'state) ast_node_processor -> 'state -> unit

val foreach_expr : 'a exp -> ('a, 'state) ast_expr_processor -> 'state -> unit
