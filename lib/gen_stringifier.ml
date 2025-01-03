open Stringifier

open Sail_ast_processor
open Sail_ast_foreach

open Utils
open Sail_utils
open Constants

open Sail_analysis

open Libsail
open Ast

type stringifier_gen_iteration_state = {
  analysis : sail_analysis_result;
  mutable stringifier : (case_name, subclause list) Hashtbl.t;
  (* Enforces the order of ast union cases on the assembly_rules hash table,
     this is necessary as hash tables aren't ordered *)
  mutable case_names : string list;
}

let get_arg_specializations analysis pats =
  let add_specialization specs i pat =
    match pat with
    | MP_aux (MP_id id, _) ->
        if Sail_analysis.is_member_of_enum analysis id then
          specs := (i, id_to_str id) :: !specs
    | MP_aux (MP_lit (L_aux (L_true, _)), _) -> specs := (i, "true") :: !specs
    | MP_aux (MP_lit (L_aux (L_false, _)), _) -> specs := (i, "false") :: !specs
    | _ -> ()
  in
  let specializations = ref [] in
  List.iteri (add_specialization specializations) pats;
  !specializations

let case_arg_to_id case_arg =
  match case_arg with MP_aux (MP_id i, _) -> Some i | _ -> None

let case_args_to_intrinsic_logic_args arg_names_to_indices args =
  let case_arg_to_intrinsic_logic_arg arg =
    match arg with
    | MP_aux (MP_id id, _) ->
        Arg_index (Hashtbl.find arg_names_to_indices (id_to_str id))
    | MP_aux (MP_vector_concat pats, _) ->
        Bv_concat
          (List.map
             (fun pat ->
               match pat with
               | MP_aux (MP_lit lit, _) ->
                   Bv_lit (bitv_literal_to_str lit, bitv_literal_size lit)
               | MP_aux (MP_typ (MP_aux (MP_id id, _), typ), _) ->
                   let arg_idx =
                     Hashtbl.find arg_names_to_indices (id_to_str id)
                   in
                   let arg_size =
                     match typ with
                     | Typ_aux (Typ_app (id, args), _) ->
                         sail_bitv_size_to_int (List.nth args 0)
                     | _ -> failwith ""
                   in
                   Arg_idx (arg_idx, arg_size)
               | _ -> failwith ""
             )
             pats
          )
    | _ -> failwith "200"
  in
  match args with
  | [MP_aux (MP_lit (L_aux (L_unit, _)), _)] -> []
  | _ -> List.map case_arg_to_intrinsic_logic_arg args

let get_case analysis pat =
  let (MPat_aux (pattern, _)) = pat in
  match pattern with
  | MPat_pat p | MPat_when (p, _) -> (
      let (MP_aux (pat, _)) = p in
      match pat with
      | MP_app (ast_case_id, union_case_args) ->
          let case_name = id_to_str ast_case_id in
          let case_ids = List.map case_arg_to_id union_case_args in
          let arg_names_to_arg_indices = Hashtbl.create 50 in
          List.iteri
            (fun idx arg_id ->
              if Option.is_some arg_id then
                Hashtbl.add arg_names_to_arg_indices
                  (id_to_str (Option.get arg_id))
                  idx
            )
            case_ids;
          let specializations =
            get_arg_specializations analysis union_case_args
          in
          assert_empty_or_length1_or_failwith specializations
            "Can't specialize an assembly clause on more than 1 argument";

          ( case_name,
            get_sole_element_or_none specializations,
            arg_names_to_arg_indices
          )
      | _ -> failwith "1"
    )

let create_bv2str name tbl arg_names_to_indices args =
  match args with
  | [arg] ->
      let arg_name = id_to_str (Option.get (case_arg_to_id arg)) in
      let arg_idx = Hashtbl.find arg_names_to_indices arg_name in
      Bitv2Str (name, arg_idx, tbl)
  | _ -> failwith ""

let create_enum2str analysis name tbl arg_names_to_indices args =
  match args with
  | [arg] ->
      let arg_id = Option.get (case_arg_to_id arg) in
      let arg_name = id_to_str arg_id in
      if is_member_of_enum analysis arg_id then
        (* optimize the table lookup by doing it and returning a literal *)
        Lit (Hashtbl.find tbl arg_name)
      else (
        (* emit the table lookup as-is *)
        let arg_idx = Hashtbl.find arg_names_to_indices arg_name in
        Enum2Str (name, arg_idx, tbl)
      )
  | _ -> failwith ""

let create_bool2str name tbl arg_names_to_indices args =
  match args with
  | [arg] -> (
      match arg with
      (* like enum2str tables generation above, optimize literal lookups into literal strings *)
      | MP_aux (MP_lit (L_aux (L_true, _)), _) -> Lit (snd tbl)
      | MP_aux (MP_lit (L_aux (L_false, _)), _) -> Lit (fst tbl)
      (* general fallback: emit a lookup by name *)
      | _ ->
          let arg_idx =
            Hashtbl.find arg_names_to_indices
              (id_to_str (Option.get (case_arg_to_id arg)))
          in
          Bool2Str (name, arg_idx, tbl)
    )
  | _ -> failwith ""

let create_struct2str name tbl arg_names_to_indices args =
  match args with
  | [arg] ->
      let arg_name = id_to_str (Option.get (case_arg_to_id arg)) in
      let arg_idx = Hashtbl.find arg_names_to_indices arg_name in
      Struct2str (name, arg_idx, tbl)
  | _ -> failwith ""

let create_to_str_from_pattern analysis arg_names_to_indices pat =
  let (MP_aux (p, _)) = pat in
  match p with
  | MP_lit (L_aux (L_string str, _)) -> Lit str
  | MP_app (id, args) ->
      let name = id_to_str id in
      let maybe_bv2str = get_bv2str_mapping analysis name in
      if Option.is_some maybe_bv2str then
        create_bv2str name (Option.get maybe_bv2str) arg_names_to_indices args
      else (
        let maybe_enum2str = get_enum2str_mapping analysis name in
        if Option.is_some maybe_enum2str then
          create_enum2str analysis name
            (Option.get maybe_enum2str)
            arg_names_to_indices args
        else (
          let maybe_bool2str = get_bool2str_mapping analysis name in
          if Option.is_some maybe_bool2str then
            create_bool2str name
              (Option.get maybe_bool2str)
              arg_names_to_indices args
          else (
            let maybe_struct2str = get_struct2str_mapping analysis name in
            if Option.is_some maybe_struct2str then
              create_struct2str name
                (Option.get maybe_struct2str)
                arg_names_to_indices args
            else
              Intrinsic_tostr_logic
                ( name,
                  case_args_to_intrinsic_logic_args arg_names_to_indices args
                )
          )
        )
      )
  | _ -> failwith ""

let create_to_strs analysis arg_names_to_indices right =
  let (MPat_aux (pat, _)) = right in
  match pat with
  | MPat_pat p | MPat_when (p, _) -> (
      let (MP_aux (pat, _)) = p in
      match pat with
      | MP_lit (L_aux (L_string str, _)) -> [Lit str]
      | MP_string_append pats ->
          List.map
            (create_to_str_from_pattern analysis arg_names_to_indices)
            pats
      | _ -> failwith "+__+"
    )

let gen_stringifier_rule state _ id _ left right =
  if id_to_str id = ast_assembly_mapping then (
    let case_name, subcase_cond, arg_names_to_indices =
      get_case state.analysis left
    in
    let tostr = create_to_strs state.analysis arg_names_to_indices right in
    let subcase = (subcase_cond, tostr) in

    (* does the case already exist ? *)
    if Hashtbl.mem state.stringifier case_name then (
      (* if yes, add a subcase to already existing subcases *)
      let subcases = Hashtbl.find state.stringifier case_name in

      (* But only if the current subcase isn't the second unspecialized subcase encountered *)
      let is_unspecialized = Option.is_none subcase_cond in
      if
        is_unspecialized
        && List.exists (fun (cond, _) -> Option.is_none cond) subcases
      then
        failwith "Error, a case can't have more than 1 unspecialized subcases"
      else (
        let subcases = subcase :: subcases in
        Hashtbl.replace state.stringifier case_name subcases
      )
    )
    else (
      (* this is the first time the case is encountered *)
      Hashtbl.add state.stringifier case_name [subcase];
      state.case_names <- case_name :: state.case_names
    )
  )

let gen_stringifier ast analysis =
  let state = { analysis; stringifier = Hashtbl.create 500; case_names = [] } in
  let stringifier_gen_processor =
    { default_processor with process_mapping_bidir_clause = gen_stringifier_rule }
  in
  foreach_node ast stringifier_gen_processor state;

  let stringifier = ref [] in
  (* No need to reverse case_names, the correct effect is achieved by
     iterating case names in reverse (as is their order in case_names)
     and creating the assembler entries also in reverse *)
  List.iter
    (fun case_name ->
      let subcases = Hashtbl.find state.stringifier case_name in
      stringifier := (case_name, List.rev subcases) :: !stringifier
    )
    state.case_names;
  !stringifier
