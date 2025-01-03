open Sail_values

type case_name = string

type bv_lit_xor_arg_idx =
  | Bv_lit of string * int (* bitv size of literal *)
  | Arg_idx of int * int (* bitv size of arg *)

type intrinsic_logic_arg =
  | Arg_index of int
  | Bv_concat of bv_lit_xor_arg_idx list

type tostr_logic =
  | Lit of string
  | Bitv2Str of string * int * bv2str_table
  | Enum2Str of string * int * enum2str_table
  | Bool2Str of string * int * bool2str_table
  | Struct2str of string * int * struct2str_table
  | Intrinsic_tostr_logic of string * intrinsic_logic_arg list

type subclause_condition = (int * string) option

type subclause = subclause_condition * tostr_logic list

type clause = case_name * subclause list

type stringifier = clause list
