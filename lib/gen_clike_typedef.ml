open Sail_ast_processor
open Sail_ast_foreach
open Sail_utils
open Utils

open Libsail
open Ast

open Clike_typedef
open Constants
open Hashset

type typedef_gen_iteration_state = {
  typenames_to_typedefs : (string, type_def_aux) Hashtbl.t;
  constructor_names_to_member_names : (string, string list) Hashtbl.t;
  previously_defined_enums : string set;
}

let collect_typedefs state node typename =
  Hashtbl.add state.typenames_to_typedefs (id_to_str typename) node

let collect_case_names_and_member_names state _ name (Pat_aux (pat_expr, _)) =
  let name_str = id_to_str_noexn name in
  let stringify_id_lowercase i = String.lowercase_ascii (id_to_str i) in
  if name_str = ast_sail_destructuring_function then (
    match pat_expr with
    | Pat_exp (P_aux (pattern, (loc, _)), _)
    | Pat_when (P_aux (pattern, (loc, _)), _, _) -> (
        match pattern with
        | P_app (case_name, [P_aux (P_tuple member_list, _)]) ->
            let case_name_as_str = stringify_id_lowercase case_name in
            let member_str_list =
              List.map
                (fun m ->
                  match m with
                  | P_aux (P_id i, _) -> stringify_id_lowercase i
                  | _ ->
                      failwith
                        "Naming ast members failed: expected all args to the \
                         union constructor to be identifiers"
                )
                member_list
            in
            Hashtbl.add state.constructor_names_to_member_names case_name_as_str
              member_str_list
        | P_app (case_name, [P_aux (P_id single_member, _)]) ->
            Hashtbl.add state.constructor_names_to_member_names
              (stringify_id_lowercase case_name)
              [stringify_id_lowercase single_member]
        | P_app (case_name, [P_aux (P_lit _, _)]) ->
            Hashtbl.add state.constructor_names_to_member_names
              (stringify_id_lowercase case_name)
              []
        | _ ->
            failwith
              ("Naming ast members failed: Expected all clauses of the ast \
                destructuring function to have a single union constructor \
                call "
              ^ stringify_sail_source_loc loc
              )
      )
  )

let split_union_into_names_and_bodies (union : type_def_aux) =
  let split_single_union_case_into_name_and_body union_case names_and_bodies =
    let (Tu_aux (Tu_ty_id (typ, id), _)) = union_case in
    let name = id_to_str id in
    let names, bodies = names_and_bodies in
    (name :: names, typ :: bodies)
  in
  match union with
  | TD_variant (_, _, cases, _) ->
      List.fold_right split_single_union_case_into_name_and_body cases ([], [])
  | _ -> invalid_arg "Expected a TD_variant, found non-variant type"

let int_to_clike_bitv_size size =
  match size with
  | 8 -> Clike_byte
  | 16 -> Clike_word
  | 32 -> Clike_dword
  | 64 -> Clike_qword
  | _ ->
      if size < 64 then Clike_bitfield size
      else
        failwith
          ("size constant too big, can't represent bitvec of size "
         ^ string_of_int size
          )

let enum_case_to_str enum_case_id =
  let str = id_to_str enum_case_id in
  add_prefix_unless_exists identifier_prefix str

let gen_clike_builtin_from_bitvec_size_expr name arg =
  Clike_builtin (name, int_to_clike_bitv_size (sail_bitv_size_to_int arg))

let gen_clike_type_from_enum enum_name name case_names =
  Clike_enum (enum_name, name, List.map enum_case_to_str case_names)

let gen_clike_type_from_app name constructor args =
  match constructor with
  | "bits" -> gen_clike_builtin_from_bitvec_size_expr name (List.nth args 0)
  | _ -> failwith ("Unsupported type constructor call " ^ constructor)

let rec gen_clike_type name typgen_info typ =
  let (Typ_aux (typ_aux, _)) = typ in
  match typ_aux with
  | Typ_id id -> gen_clike_type_from_id name typgen_info id
  | Typ_app (id, args) -> gen_clike_type_from_app name (id_to_str id) args
  | Typ_tuple types -> gen_clike_type_from_tuple name typgen_info types
  | _ -> invalid_arg "Cant translate this type to the C type system"

and gen_clike_type_from_id name typgen_info id =
  let typ_name = id_to_str id in
  match typ_name with
  | "unit" -> Clike_void
  | "bool" -> Clike_builtin (name, Clike_bitfield 1)
  | _ -> (
      let typ_def = Hashtbl.find typgen_info.typenames_to_typedefs typ_name in
      match typ_def with
      | TD_abbrev (_, _, typarg) ->
          gen_clike_type_from_abbrev name typgen_info typarg
      | TD_enum (id, case_names, _) ->
          let enum_name = id_to_str id in
          if set_contains typgen_info.previously_defined_enums enum_name then
            Clike_typename ("enum " ^ enum_name, name)
          else (
            set_add typgen_info.previously_defined_enums enum_name;
            gen_clike_type_from_enum enum_name name case_names
          )
      | TD_record (record_id, _, members, _) ->
          gen_clike_type_from_record name typgen_info record_id members
      | TD_variant (_, _, members, _) -> (
          match members with
          | [Tu_aux (Tu_ty_id (typ, _), _)] ->
              gen_clike_type name typgen_info typ
          | _ ->
              failwith
                ("Variant " ^ typ_name
               ^ " is too complex to be translated into C"
                )
        )
      | _ ->
          invalid_arg
            ("Type " ^ typ_name ^ " cant be translated into a C equivalent")
    )

and gen_clike_type_from_abbrev name typgen_info typarg =
  match typarg with
  | A_aux (A_typ typ, _) -> gen_clike_type name typgen_info typ
  | _ -> failwith "Type arguments other than A_typ are not supported"

and gen_clike_type_from_tuple name typgen_info types =
  let tuple_members_names =
    Hashtbl.find typgen_info.constructor_names_to_member_names name
  in
  let clike_types =
    List.mapi
      (fun i t -> gen_clike_type (List.nth tuple_members_names i) typgen_info t)
      types
  in
  Clike_struct ("", name, clike_types)

and gen_clike_type_from_record name typgen_info record_id members =
  Clike_struct
    ( id_to_str record_id,
      name,
      List.map
        (fun (ty, id) -> gen_clike_type (id_to_str id) typgen_info ty)
        members
    )

let gen_clike_typedef ast_typename typgen_info =
  let ast_type = Hashtbl.find typgen_info.typenames_to_typedefs ast_typename in
  let case_names, case_bodies = split_union_into_names_and_bodies ast_type in
  let clike_bodies =
    List.mapi
      (fun i t ->
        let name = String.lowercase_ascii (List.nth case_names i) in
        gen_clike_type name typgen_info t
      )
      case_bodies
  in
  let prefixed_case_names =
    List.map (add_prefix_unless_exists identifier_prefix) case_names
  in
  Clike_struct
    ( ast_typename,
      "",
      [
        Clike_enum
          ("", ast_typename ^ generated_ast_enum_suffix, prefixed_case_names);
        Clike_union
          ("", ast_typename ^ generated_ast_payload_suffix, clike_bodies);
      ]
    )

let filter_primitive_union_cases clike_struct =
  let cases = Hashtbl.create 50 in
  match clike_struct with
  | Clike_struct (_, _, [_; Clike_union (_, _, members)]) ->
      List.iter
        (fun clike_type ->
          match clike_type with
          | Clike_builtin (name, _) -> set_add cases name
          | _ -> ()
        )
        members;
      cases
  | _ -> cases

type typedef_walker = {
  case_names_to_member_names : (string, string list) Hashtbl.t;
  primitive_cases : string set;
  (* mutable state *)
  mutable curr_case : string;
  mutable curr_case_remaining_member : string list;
  mutable curr_case_is_primitive : bool;
  mutable curr_primitive_case_already_walked : bool;
}

let set_walker_case walker case =
  walker.curr_case <- String.lowercase_ascii case;
  walker.curr_case_remaining_member <-
    Hashtbl.find walker.case_names_to_member_names walker.curr_case;
  walker.curr_case_is_primitive <-
    set_contains walker.primitive_cases walker.curr_case;
  walker.curr_primitive_case_already_walked <- false;
  ast_sail_def_name ^ generated_ast_enum_suffix

let walk walker =
  if walker.curr_case_is_primitive then
    if walker.curr_primitive_case_already_walked then None
    else (
      walker.curr_primitive_case_already_walked <- true;
      let path =
        ast_sail_def_name ^ generated_ast_payload_suffix ^ "."
        ^ walker.curr_case
      in
      Some path
    )
  else (
    match walker.curr_case_remaining_member with
    | [] -> None
    | next :: rest ->
        walker.curr_case_remaining_member <- rest;
        let path =
          ast_sail_def_name ^ generated_ast_payload_suffix ^ "."
          ^ walker.curr_case ^ "." ^ next
        in
        Some path
  )

let get_member_path walker arg_idx =
  if walker.curr_case_is_primitive then
    if arg_idx != 0 then None
    else (
      let path =
        ast_sail_def_name ^ generated_ast_payload_suffix ^ "."
        ^ walker.curr_case
      in
      Some path
    )
  else if arg_idx >= List.length walker.curr_case_remaining_member then None
  else (
    let member =
      List.nth
        (Hashtbl.find walker.case_names_to_member_names walker.curr_case)
        arg_idx
    in
    let path =
      ast_sail_def_name ^ generated_ast_payload_suffix ^ "." ^ walker.curr_case
      ^ "." ^ member
    in
    Some path
  )

let gen_def ast =
  let state =
    {
      typenames_to_typedefs = Hashtbl.create 100;
      constructor_names_to_member_names = Hashtbl.create 50;
      previously_defined_enums = Hashtbl.create 50;
    }
  in
  let processor =
    {
      default_processor with
      process_typedef = collect_typedefs;
      process_function_clause = collect_case_names_and_member_names;
    }
  in
  foreach_node ast processor state;
  let typedef = gen_clike_typedef ast_sail_def_name state in
  let walker =
    {
      case_names_to_member_names = state.constructor_names_to_member_names;
      primitive_cases = filter_primitive_union_cases typedef;
      curr_case = "";
      curr_case_remaining_member = [];
      curr_case_is_primitive = false;
      curr_primitive_case_already_walked = false;
    }
  in

  (typedef, walker)
