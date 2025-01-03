open Libsail
open Type_check
open Ast_defs

open Decoder
open Decode_procedure
open Sail_analysis

val gen_decoder : string -> (tannot, env) ast -> sail_analysis_result -> decoder

val gen_decode_proc : decoder -> decode_procedure
