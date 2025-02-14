open Sail_ast_foreach
open Sail_ast_processor
open Sail_utils

open Utils
open Constants
open Sail_values
open Decoder
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
          print_endline ("\n+++++++++++++++++++++++++" ^ case_name);
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

let create_conditions_from_pat state p arg_bindings =
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
                  when id_to_str id = "bitvector" || id_to_str id = "bits" ->
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
                      "Unsupported mapping pattern, multiple arguments are not \
                       supported"
              in
              let bv_to_enum =
                get_bv2enum_mapping state.analysis mapping_name
              in
              match bv_to_enum with
              | Some bv2enum ->
                  let maybe_size = Hashtbl.find arg_bindings arg_name in
                  let size = get_some_or_failwith maybe_size "UNREACHABLE" in
                  Map_bind (size, bv2enum, arg_name)
              | None -> (
                  match get_bv2struct_mapping state.analysis mapping_name with
                  | Some (struct_name, bv_to_struct) ->
                      let maybe_size = Hashtbl.find arg_bindings arg_name in
                      let size =
                        get_some_or_failwith maybe_size "UNREACHABLE"
                      in
                      Struct_map_bind (size, struct_name, bv_to_struct, arg_name)
                  | None ->
                      failwith
                        ("Mapping " ^ mapping_name
                       ^ " is neither a bv<->enum nor a bv<->struct mapping"
                        )
                )
            )
          | _ ->
              failwith
                ("Unsupported decode condition @ "
                ^ stringify_sail_source_loc loc
                )
        )
        (List.rev pats)
  | _ -> failwith "Expected a vector expression"

let exp_to_operand (E_aux (e, _)) =
  match e with
  | E_id varname -> Some (Id_or_funcall (id_to_str varname, []))
  | E_lit (L_aux (L_num n, _)) -> Some (Number (Nat_big_num.to_int n))
  | E_app (funname, args) -> (
      try
        let argnames =
          List.map
            (fun (E_aux (a, _)) ->
              match a with
              | E_id varname -> id_to_str varname
              | _ -> invalid_arg "Args must be all single identifiers"
            )
            args
        in
        Some (Id_or_funcall (id_to_str funname, argnames))
      with Invalid_argument _ -> None
    )
  | _ -> None

let destructure_exps_to_int_operands es =
  match es with
  | [o1; o2] -> (
      match (exp_to_operand o1, exp_to_operand o2) with
      | None, _ -> None
      | _, None -> None
      | Some op1, Some op2 -> Some (op1, op2)
    )
  | _ -> None

let flatten_bv_concat_args a1 a2 =
  let rec flatten_arg (E_aux (a, _)) =
    match a with
    | E_id i -> [id_to_str i]
    | E_lit (L_aux (L_bin _, _) as lit) | E_lit (L_aux (L_hex _, _) as lit) ->
        [bitv_literal_to_str lit]
    | E_app (i, [a1; a2]) when id_to_str i = "bitvector_concat" ->
        flatten_arg a1 @ flatten_arg a2
    | _ -> failwith "UNREACHABLE"
  in
  flatten_arg a1 @ flatten_arg a2

let destructure_exps_to_bitv_operands es =
  match es with
  | [E_aux (E_id id, _); E_aux (E_lit l, _)]
  | [E_aux (E_lit l, _); E_aux (E_id id, _)] -> (
      let name = id_to_str id in
      match l with
      | L_aux (L_hex _, _) | L_aux (L_bin _, _) ->
          Some ([name], bitv_literal_to_str l)
      | _ -> None
    )
  | [E_aux (E_id id1, _); E_aux (E_id id2, _)] ->
      Some ([id_to_str id1], id_to_str id2)
  | [E_aux (E_app (i, args), _); op] | [op; E_aux (E_app (i, args), _)] ->
      if id_to_str i = "bitvector_concat" then (
        let operands =
          flatten_bv_concat_args (List.nth args 0) (List.nth args 1)
        in
        let (E_aux (op, _)) = op in
        match op with
        | E_id i -> Some (operands, id_to_str i)
        | E_lit (L_aux (L_bin _, _) as lit) | E_lit (L_aux (L_hex _, _) as lit)
          ->
            Some (operands, bitv_literal_to_str lit)
        | _ -> None
      )
      else None
  | _ -> None

let destructure_exps_to_bitv_access_and_bit es =
  match es with
  | [E_aux (E_app (i, [a; b]), _); E_aux (E_lit l, _)]
  | [E_aux (E_lit l, _); E_aux (E_app (i, [a; b]), _)]
    when id_to_str i = "bitvector_access" -> (
      match (a, b) with
      | E_aux (E_id varname, _), E_aux (E_lit (L_aux (L_num n, _)), _)
      | E_aux (E_lit (L_aux (L_num n, _)), _), E_aux (E_id varname, _) -> (
          let name = id_to_str varname in
          match l with
          | L_aux (L_one, _) -> Some (name, Nat_big_num.to_int n, true)
          | L_aux (L_zero, _) -> Some (name, Nat_big_num.to_int n, false)
          | _ -> None
        )
      | _ -> None
    )
  | _ -> None

let rec create_guard (E_aux (e, (l, _))) =
  match e with
  | E_app (id, args) -> (
      match id_to_str_noexn id with
      (* | *)
      | "or_bool" ->
          Or (create_guard (List.nth args 0), create_guard (List.nth args 1))
      (* & *)
      | "and_bool" ->
          And (create_guard (List.nth args 0), create_guard (List.nth args 1))
      (* <= *)
      | "lteq_int" -> (
          match destructure_exps_to_int_operands args with
          | Some (op1, op2) -> Less_eq_or_eq (op1, op2)
          | None ->
              failwith
                ("Unsupported <= (less-than-or-equals) call @ "
                ^ stringify_sail_source_loc l
                )
        )
      (* == *)
      | "eq_int" -> (
          match destructure_exps_to_int_operands args with
          | Some (op1, op2) -> Eq_int (op1, op2)
          | None ->
              failwith
                ("Unsupported == (is-equals) with int arguments call @ "
                ^ stringify_sail_source_loc l
                )
        )
      (* < *)
      | "lt_int" -> (
          match destructure_exps_to_int_operands args with
          | Some (op1, op2) -> Less_eq_int (op1, op2)
          | _ ->
              failwith
                ("unsupported < (less-than) call @ "
                ^ stringify_sail_source_loc l
                )
        )
      (* >= *)
      | "gteq_int" -> (
          match destructure_exps_to_int_operands args with
          | Some (op1, op2) -> Not (Less_eq_int (op1, op2))
          | _ ->
              failwith
                ("unsupported < (less-than) call @ "
                ^ stringify_sail_source_loc l
                )
        )
      (* != *)
      | "neq_bits" -> (
          match destructure_exps_to_bitv_operands args with
          | Some (ops, op2) -> Not (Eq_bv (ops, op2))
          | None ->
              failwith
                ("Unsupported != (is-not-equals) with bitvec arguments call @ "
                ^ stringify_sail_source_loc l
                )
        )
      (* == *)
      | "eq_bit" -> (
          match destructure_exps_to_bitv_access_and_bit args with
          | Some (name, idx, b) -> Eq_bit (name, idx, b)
          | None ->
              failwith
                ("Unsupported == (is-equals) with bit arguments call @ "
                ^ stringify_sail_source_loc l
                )
              (* == *)
        )
      | "eq_bits" -> (
          match destructure_exps_to_bitv_operands args with
          | Some (ops, op2) -> Eq_bv (ops, op2)
          | None ->
              failwith
                ("Unsupported == (is-equals) with bitvec arguments call @ "
                ^ stringify_sail_source_loc l
                )
        )
      (* less than for bitvecs *)
      | "<_u" -> (
          match destructure_exps_to_bitv_operands args with
          | Some ([op1], op2) -> Less_eq_bv (op1, op2)
          | _ ->
              failwith
                ("Unsupported <_u (is-less-than-or-equals-unsigned) with \
                  bitvec arguments call @ "
                ^ stringify_sail_source_loc l
                )
        )
      | name -> (
          try
            print_endline ("\n@@@@@@@@@@@@@@@@ FUNC " ^ name ^ " @@@@@@@@@@@\n");
            let args =
              List.map
                (fun (E_aux (a, _)) ->
                  match a with
                  | E_id i -> [id_to_str i]
                  | E_lit (L_aux (L_unit, _)) -> raise (Invalid_argument "unit")
                  | E_lit (L_aux (L_true, _)) | E_lit (L_aux (L_one, _)) ->
                      ["1"]
                  | E_lit (L_aux (L_false, _)) | E_lit (L_aux (L_zero, _)) ->
                      ["0"]
                  | E_lit (L_aux (L_num n, _)) -> [Nat_big_num.to_string n]
                  | E_lit (L_aux (L_bin _, _) as l)
                  | E_lit (L_aux (L_hex _, _) as l) ->
                      [bitv_literal_to_str l]
                  | E_vector elems ->
                      List.map
                        (fun e ->
                          match e with
                          | E_aux (E_id i, _) -> id_to_str i
                          | _ ->
                              failwith
                                "Unsupported argument in boolean call: List \
                                 too complex"
                        )
                        elems
                  | _ ->
                      failwith
                        ("Unsupported boolean call '" ^ name ^ "' @ "
                        ^ stringify_sail_source_loc l
                        )
                )
                args
            in

            Boolfun (name, List.flatten args)
          with Invalid_argument _ -> Boolfun (name, [])
        )
    )
  | E_typ (t, e) -> create_guard e
  | _ ->
      failwith
        ("Unsupported boolean expression @ " ^ stringify_sail_source_loc l)

let create_conditions state r arg_bindings =
  let (MPat_aux (right, (l, _))) = r in
  match right with
  | MPat_pat p -> (create_conditions_from_pat state p arg_bindings, True)
  | MPat_when (p, e) ->
      let conds = create_conditions_from_pat state p arg_bindings in
      print_endline
        ("HELLO:::: FOUND PREDICATES @ " ^ stringify_sail_source_loc l);
      let (E_aux (exp, (l, _))) = e in
      ( match exp with
      | E_app (id, args) ->
          print_endline
            ("\nAPPLICATION gAURD ----------------------------- " ^ id_to_str id
           ^ ", num ARGS IS "
            ^ string_of_int (List.length args)
            ^ "(@"
            ^ stringify_sail_source_loc l
            ^ ")"
            )
      | _ -> print_endline "OTHER GAURD"
      );
      (conds, create_guard e)

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
    let conditions, guards = create_conditions state right bindings in
    print_endline
      "\n\n\
       &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&DONE&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&\n\n";
    state.decode_rules <-
      (conditions, guards, consequences) :: state.decode_rules
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
