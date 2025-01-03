open Sail_ast_foreach
open Sail_ast_processor
open Sail_utils
open Utils
open Constants
open Sail_values
open Decoder
open Decode_procedure
open Sail_analysis

open Libsail
open Ast

type decoder_gen_iteration_state = {
  analysis : sail_analysis_result;
  (* The length of the bitstream input to the decoder
     Of type option because it initializes to None, then is inferred from the type of the decode mapping *)
  mutable instr_length : int option;
  (* The final output of the decoder generation process, gradually built up mutably during iteration of the ast*)
  mutable decode_rules : decoder;
}

let lit_to_consequence_body lit =
  let (L_aux (literal, loc)) = lit in
  match literal with
  | L_bin _ | L_hex _ -> Push (Bv_const (bitv_literal_to_str lit))
  | L_true -> Push (Bool_const true)
  | L_false -> Push (Bool_const false)
  | _ -> failwith ("Unsupported literal @ " ^ stringify_sail_source_loc loc)

let idstr_to_consequence_body idstr =
  if idstr = String.capitalize_ascii idstr then Push (Enum_lit idstr)
  else Push (Binding idstr)

let vec_concat_to_consequence_body slices =
  Concat_push
    (List.map
       (fun s ->
         let (MP_aux (slice, _)) = s in
         match slice with
         | MP_lit (L_aux (lit, _) as l) -> (
             match lit with
             | L_bin _ | L_hex _ -> Bv_const (bitv_literal_to_str l)
             | _ -> failwith "UNREACHABLE"
           )
         | MP_id i -> Binding (id_to_str i)
         | MP_typ (MP_aux (MP_id id, _), _) -> Binding (id_to_str id)
         | _ -> failwith "UNREACHABLE"
       )
       (List.rev slices)
    )

let bind_args_and_create_consequences state l =
  let (MPat_aux (left, _)) = l in
  let bindings = Hashtbl.create 20 in
  match left with
  | MPat_pat p | MPat_when (p, _) -> (
      let (MP_aux (pat, _)) = p in
      match pat with
      | MP_app (union_case_id, union_case_args) ->
          let case_name = id_to_str union_case_id in
          let bodies =
            match union_case_args with
            | [MP_aux (MP_lit (L_aux (L_unit, _)), _)] -> []
            | _ ->
                List.mapi
                  (fun i a ->
                    let (MP_aux (arg, _)) = a in
                    match arg with
                    | MP_lit lit -> lit_to_consequence_body lit
                    | MP_id id ->
                        let n = id_to_str id in
                        let bitv_size =
                          get_case_arg_size state.analysis case_name i
                        in
                        Hashtbl.add bindings n bitv_size;
                        idstr_to_consequence_body n
                    | MP_vector_concat slices ->
                        vec_concat_to_consequence_body slices
                    | _ ->
                        failwith
                          ("Unsupported pattern in argument " ^ string_of_int i
                         ^ " to " ^ case_name
                          )
                  )
                  union_case_args
          in
          (bindings, (Assign_node_type case_name, bodies))
      | _ ->
          failwith
            "Left pattern of the ast decoding mapping must be a union \
             constructor"
    )

let create_conditions state r arg_bindings =
  let (MPat_aux (right, _)) = r in
  match right with
  | MPat_pat p | MPat_when (p, _) -> (
      let (MP_aux (pat, _)) = p in
      match pat with
      | MP_id id -> [Bind (Option.get state.instr_length, id_to_str id)]
      | MP_vector_concat pats ->
          List.map
            (fun p ->
              let (MP_aux (pat, (loc, _))) = p in
              match pat with
              | MP_id id ->
                  let idstr = id_to_str id in
                  let maybe_size = Hashtbl.find arg_bindings idstr in
                  let size = get_some_or_failwith maybe_size "UNREACHABLE" in
                  Bind (size, idstr)
              | MP_typ (pat, typ) ->
                  let size =
                    match typ with
                    | Typ_aux (Typ_app (id, args), _)
                      when id_to_str id = "bitvector" || id_to_str id = "bits"
                      ->
                        sail_bitv_size_to_int (List.nth args 0)
                    | Typ_aux (Typ_id id, _) ->
                        let sz =
                          get_size_of_bv_synonym state.analysis (id_to_str id)
                        in
                        get_some_or_failwith sz
                          "Type annotation is a named type not synonymous with \
                           bitvec "
                    | _ ->
                        failwith
                          ("Type annotation cant be non-bitvec @ "
                          ^ stringify_sail_source_loc loc
                          )
                  in
                  let idstr =
                    match pat with
                    | MP_aux (MP_id id, _) -> id_to_str id
                    | _ -> failwith "Cant annotate non-id"
                  in
                  Bind (size, idstr)
              | MP_lit lit ->
                  let const = bitv_literal_to_str lit in
                  let size = bitv_literal_size lit in
                  Assert (size, const)
              | MP_app (id, args) -> (
                  let mapping_name = id_to_str id in
                  let arg_name =
                    match args with
                    | [MP_aux (MP_id i, _)] -> id_to_str i
                    | _ ->
                        failwith
                          "Unsupported mapping pattern, multiple arguments are \
                           not supported"
                  in
                  let bv_to_enum =
                    get_bv2enum_mapping state.analysis mapping_name
                  in
                  match bv_to_enum with
                  | Some bv2enum ->
                      let maybe_size = Hashtbl.find arg_bindings arg_name in
                      let size =
                        get_some_or_failwith maybe_size "UNREACHABLE"
                      in
                      Map_bind (size, bv2enum, arg_name)
                  | None -> (
                      match
                        get_bv2struct_mapping state.analysis mapping_name
                      with
                      | Some (struct_name, bv_to_struct) ->
                          let maybe_size = Hashtbl.find arg_bindings arg_name in
                          let size =
                            get_some_or_failwith maybe_size "UNREACHABLE"
                          in
                          Struct_map_bind
                            (size, struct_name, bv_to_struct, arg_name)
                      | None ->
                          failwith
                            ("Mapping " ^ mapping_name
                           ^ " is neither a bv<->enum nor a bv<->struct mapping"
                            )
                    )
                )
              | _ -> failwith ""
            )
            (List.rev pats)
      | _ -> failwith "-"
    )

let calculate_instr_length typ =
  let errmsg = "Cant calculate instruction length from ast decode mapping" in

  match typ with
  | Typ_aux (Typ_bidir (t1, t2), _) -> (
      let (Typ_aux (type1, _)) = t1 in
      let (Typ_aux (type2, _)) = t2 in
      match (type1, type2) with
      | Typ_app (id, args), _ when id_to_str id = "bitvector" ->
          sail_bitv_size_to_int (List.nth args 0)
      | _, Typ_app (id, args) when id_to_str id = "bitvector" ->
          sail_bitv_size_to_int (List.nth args 0)
      | _ -> failwith errmsg
    )
  | _ -> failwith errmsg

let calculate_instr_len state _ id _ typ =
  if id_to_str_noexn id = ast_decode_mapping then (
    assert (Option.is_none state.instr_length);
    state.instr_length <- Some (calculate_instr_length typ)
  )

let gen_decode_rule decode_mappig_name state _ id _ left right =
  if id_to_str id = decode_mappig_name then (
    let bindings, consequences = bind_args_and_create_consequences state left in
    let conditions = create_conditions state right bindings in
    state.decode_rules <- (conditions, consequences) :: state.decode_rules
  )

let gen_decoder decode_mappig_name ast analysis =
  let state = { analysis; decode_rules = []; instr_length = None } in
  let decoder_gen_processor =
    {
      default_processor with
      process_mapping_bidir_clause = gen_decode_rule decode_mappig_name;
      process_val = calculate_instr_len;
    }
  in
  foreach_node ast decoder_gen_processor state;
  state.decode_rules <- List.rev state.decode_rules;
  state.decode_rules

(***********************************************************************************************)
(************************** Decoder transformation to imperative logic *************************)
(***********************************************************************************************)

let gen_assert_boolean_exprs offsets (i, rule_cond) =
  match rule_cond with
  | Assert (_, bval) ->
      let starting = List.nth offsets i in
      let ending = List.nth offsets (i + 1) in
      Some (Is_eq (Binstr_slice (starting, ending), bval))
  | _ -> None

let gen_bind_stmts offsets (i, rule_cond) =
  match rule_cond with
  | Bind (_, var) ->
      Some
        (Init (var, Binstr_slice (List.nth offsets i, List.nth offsets (i + 1))))
  | _ -> None

let calculate_offsets conds =
  let rev_offsets =
    List.fold_left
      (fun roffs cond ->
        match cond with
        | Assert (len, _)
        | Bind (len, _)
        | Map_bind (len, _, _)
        | Struct_map_bind (len, _, _, _) -> (
            match roffs with
            | curr :: _ -> (len + curr) :: roffs
            | [] -> failwith "UNREACHABLE"
          )
      )
      [0] conds
  in
  List.rev rev_offsets

let gen_mapbind_stmt offsets (i, cond) body =
  let slice = Binstr_slice (List.nth offsets i, List.nth offsets (i + 1)) in
  match cond with
  | Map_bind (_, map_tbl, var_name) ->
      let values =
        Hashtbl.fold
          (fun bv_val enum_val vs -> (bv_val, enum_val) :: vs)
          map_tbl []
      in
      let switch = Switch_assign (var_name, slice, values) in
      Block [switch; If (Is_enum_var_valid var_name, body)]
  | Struct_map_bind (_, struct_name, map_tbl, var_name) ->
      let values =
        Hashtbl.fold
          (fun bv_val kvs accumulator -> (bv_val, kvs) :: accumulator)
          map_tbl []
      in
      let switch =
        Switch_assign_struct (struct_name, var_name, slice, values)
      in
      Block [switch; If (Is_struct_var_valid var_name, body)]
  | _ -> body

let gen_stmt_from_conditions rule_name conds conseq_stmts =
  let offsets = calculate_offsets conds in
  let indices = List.mapi (fun i _ -> i) conds in
  let conds_indexed = List.combine indices conds in
  let assert_exprs =
    List.filter_map (gen_assert_boolean_exprs offsets) conds_indexed
  in
  let bind_stmts = List.filter_map (gen_bind_stmts offsets) conds_indexed in
  let mapbind_stmt =
    List.fold_right (gen_mapbind_stmt offsets) conds_indexed
      (Block (List.append bind_stmts conseq_stmts))
  in
  match assert_exprs with
  | [] -> Block [Start_rule rule_name; mapbind_stmt; End_rule]
  | _ ->
      Block
        [Start_rule rule_name; If (And assert_exprs, mapbind_stmt); End_rule]

let gen_stmt_from_consequences consequences =
  let head, bodies = consequences in
  let (Assign_node_type case_name) = head in
  let body_stmts =
    List.map
      (fun body ->
        match body with
        | Push v -> Set_ast_next_case_member (Val v)
        | Concat_push vs ->
            let literals =
              List.map
                (fun v ->
                  match v with
                  | Bv_const bvc -> Literal bvc
                  | Binding name -> Id name
                  | _ ->
                      failwith
                        "Type Error: concat undefined for non-bitvec values"
                )
                vs
            in
            Set_ast_next_case_member (Exp (Concat literals))
      )
      bodies
  in
  (List.append (Set_ast_case case_name :: body_stmts) [Ret_ast], case_name)

let gen_stmt_from_rule rule =
  let conditions, consequences = rule in
  let conseq_stmts, name = gen_stmt_from_consequences consequences in
  gen_stmt_from_conditions name conditions conseq_stmts

let gen_decode_proc decoder = Proc (Block (List.map gen_stmt_from_rule decoder))
