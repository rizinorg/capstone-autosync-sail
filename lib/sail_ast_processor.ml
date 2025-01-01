open Libsail
open Ast

type ('a, 'state) ast_node_processor = {
  process_typedef : 'state -> type_def_aux -> id -> unit;
  process_abbrev : 'state -> type_def_aux -> id -> typquant -> typ_arg -> unit;
  process_record :
    'state -> type_def_aux -> id -> typquant -> (typ * id) list -> bool -> unit;
  process_union :
    'state -> type_def_aux -> id -> typquant -> type_union list -> bool -> unit;
  process_union_clause : 'state -> type_union_aux -> id -> id -> typ -> unit;
  process_enum : 'state -> type_def_aux -> id -> id list -> bool -> unit;
  process_bitfield :
    'state -> type_def_aux -> id -> typ -> (id * index_range) list -> unit;
  process_function_clause :
    'state -> 'a funcl_aux -> id -> 'a pexp_funcl -> unit;
  process_mapping :
    'state -> 'a mapdef_aux -> id -> tannot_opt -> 'a mapcl list -> unit;
  process_mapping_bidir_clause :
    'state -> 'a mapcl_aux -> id -> tannot_opt -> 'a mpexp -> 'a mpexp -> unit;
  process_val : 'state -> val_spec_aux -> id -> typquant -> typ -> unit;
}

let default_processor =
  {
    process_typedef = (fun _ _ _ -> ());
    process_abbrev = (fun _ _ _ _ _ -> ());
    process_record = (fun _ _ _ _ _ _ -> ());
    process_union = (fun _ _ _ _ _ _ -> ());
    process_union_clause = (fun _ _ _ _ _ -> ());
    process_enum = (fun _ _ _ _ _ -> ());
    process_bitfield = (fun _ _ _ _ _ -> ());
    process_function_clause = (fun _ _ _ _ -> ());
    process_mapping = (fun _ _ _ _ _ -> ());
    process_mapping_bidir_clause = (fun _ _ _ _ _ _ -> ());
    process_val = (fun _ _ _ _ _ -> ());
  }

type ('a, 'state) ast_expr_processor = {
  process_block : 'state -> 'a exp list -> unit;
  process_id : 'state -> id -> unit;
  process_lit : 'state -> lit -> unit;
  process_typ : 'state -> typ -> 'a exp -> unit;
  process_app : 'state -> id -> 'a exp list -> unit;
  process_app_infix : 'state -> 'a exp -> id -> 'a exp -> unit;
  process_tuple : 'state -> 'a exp list -> unit;
  process_if : 'state -> 'a exp -> 'a exp -> 'a exp -> unit;
  process_loop :
    'state -> loop -> 'a internal_loop_measure -> 'a exp -> 'a exp -> unit;
  process_for :
    'state -> id -> 'a exp -> 'a exp -> 'a exp -> order -> 'a exp -> unit;
  process_vector : 'state -> 'a exp list -> unit;
  process_vector_access : 'state -> 'a exp -> 'a exp -> unit;
  process_vector_subrange : 'state -> 'a exp -> 'a exp -> 'a exp -> unit;
  process_vector_update : 'state -> 'a exp -> 'a exp -> 'a exp -> unit;
  process_vector_update_subrange :
    'state -> 'a exp -> 'a exp -> 'a exp -> 'a exp -> unit;
  process_vector_append : 'state -> 'a exp -> 'a exp -> unit;
  process_list : 'state -> 'a exp list -> unit;
  process_cons : 'state -> 'a exp -> 'a exp -> unit;
  process_struct : 'state -> 'a fexp list -> unit;
  process_struct_update : 'state -> 'a exp -> 'a fexp list -> unit;
  process_field : 'state -> 'a exp -> id -> unit;
  process_match : 'state -> 'a exp -> 'a pexp list -> unit;
  process_let : 'state -> 'a letbind -> 'a exp -> unit;
  process_assign : 'state -> 'a lexp -> 'a exp -> unit;
  process_return : 'state -> 'a exp -> unit;
  process_exit : 'state -> 'a exp -> unit;
  process_ref : 'state -> id -> unit;
  process_throw : 'state -> 'a exp -> unit;
  process_try : 'state -> 'a exp -> 'a pexp list -> unit;
  process_assert : 'state -> 'a exp -> 'a exp -> unit;
  process_var : 'state -> 'a lexp -> 'a exp -> 'a exp -> unit;
}

let default_expr_processor =
  {
    process_block = (fun _ _ -> ());
    process_id = (fun _ _ -> ());
    process_lit = (fun _ _ -> ());
    process_typ = (fun _ _ _ -> ());
    process_app = (fun _ _ _ -> ());
    process_app_infix = (fun _ _ _ _ -> ());
    process_tuple = (fun _ _ -> ());
    process_if = (fun _ _ _ _ -> ());
    process_loop = (fun _ _ _ _ _ -> ());
    process_for = (fun _ _ _ _ _ _ _ -> ());
    process_vector = (fun _ _ -> ());
    process_vector_access = (fun _ _ _ -> ());
    process_vector_subrange = (fun _ _ _ _ -> ());
    process_vector_update = (fun _ _ _ _ -> ());
    process_vector_update_subrange = (fun _ _ _ _ _ -> ());
    process_vector_append = (fun _ _ _ -> ());
    process_list = (fun _ _ -> ());
    process_cons = (fun _ _ _ -> ());
    process_struct = (fun _ _ -> ());
    process_struct_update = (fun _ _ _ -> ());
    process_field = (fun _ _ _ -> ());
    process_match = (fun _ _ _ -> ());
    process_let = (fun _ _ _ -> ());
    process_assign = (fun _ _ _ -> ());
    process_return = (fun _ _ -> ());
    process_exit = (fun _ _ -> ());
    process_ref = (fun _ _ -> ());
    process_throw = (fun _ _ -> ());
    process_try = (fun _ _ _ -> ());
    process_assert = (fun _ _ _ -> ());
    process_var = (fun _ _ _ _ -> ());
  }
