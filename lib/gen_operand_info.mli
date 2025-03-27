open Sail_analysis

open Libsail
open Type_check
open Ast_defs

open Gen_operand_info_defs

val gen_operand_info : (tannot, env) ast -> sail_analysis_result -> operand_info
