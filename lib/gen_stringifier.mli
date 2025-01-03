open Stringifier
open Sail_analysis

open Libsail
open Type_check
open Ast_defs

val gen_stringifier : (tannot, env) ast -> sail_analysis_result -> stringifier
