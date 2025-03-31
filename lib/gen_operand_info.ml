open Libsail
open Ast

open Sail_ast_foreach
open Sail_ast_processor
open Sail_utils
open Sail_analysis

open Utils

open Hashset

open Gen_operand_info_defs

type 'a operand_gen_iteration_state = {
  analysis : sail_analysis_result;
  (* operand details, generated incrementally during iteration of AST *)
  op_info : operand_info;
}

let are_operand_lists_equal ops1 ops2 =
  if List.length ops1 <> List.length ops2 then false
  else
    List.combine ops1 ops2
    |> List.map (fun (Reg (i1, f1, a1), Reg (i2, f2, a2)) ->
           i1 = i2 && f1 = f2 && a1 = a2
       )
    |> List.for_all (fun eq -> eq)

let bind_regidx_args reg_indices_members arg_names =
  let binding = Hashtbl.create (List.length reg_indices_members) in
  List.iteri
    (fun i arg_name ->
      if List.mem i reg_indices_members then Hashtbl.add binding arg_name i
    )
    arg_names;
  binding

let rec infer_regaccess_from_regname name =
  let get_maybe_regaccess n =
    let is_source = str_starts_with "rs" n || str_starts_with "vs" n in
    let is_destination = str_starts_with "rd" n || str_ends_with "d" n in
    if is_source && is_destination then Some Read_and_Write
    else if is_source then Some Read
    else if is_destination then Some Write
    else None
  in
  if String.contains name '_' then (
    let sub_names = String.split_on_char '_' name in
    let accesses = sub_names |> List.filter_map get_maybe_regaccess in
    if List.length accesses = 0 then
      failwith
        ("Couldn't infer register access (Read, Write, or bot) from register \
          name: " ^ name
       ^ ", register is an underscored name (i.e. x_y_..._z) where none of the \
          parts fit the known patterns"
        )
    else if List.for_all (fun a -> a = Read) accesses then Read
    else if List.for_all (fun a -> a = Write) accesses then Write
    else Read_and_Write
  )
  else (
    match get_maybe_regaccess name with
    | Some access -> access
    | _ ->
        failwith
          ("Can't infer register access (Read, Write, or both) from register \
            name: " ^ name
         ^ ", Register name doesn't fit into any known pattern (known \
            patterns: rs*, vs* for sources, *d for destinations)"
          )
  )
let infer_registers state _ fun_id func =
  let infer_registerfile_for_typename case_name arg_names regindex_typename =
    let arg_getter_by_typename =
      get_all_case_members_of_type_named state.analysis case_name
    in
    let reg_indices_members = arg_getter_by_typename regindex_typename in
    let bound_args = bind_regidx_args reg_indices_members arg_names in
    let regfile =
      List.assoc regindex_typename
        [
          ("regidx", Base);
          ("cregidx", Base_or_Float);
          ("fregidx", Float_or_Double);
          ("vregidx", Vector);
        ]
    in
    let reg_arg_names = List.of_seq (Hashtbl.to_seq_keys bound_args) in
    reg_arg_names
    |> List.map (fun regname ->
           let index = Hashtbl.find bound_args regname in
           let regacess = infer_regaccess_from_regname regname in
           Reg (index, regfile, regacess)
       )
  in
  let fun_name = id_to_str_noexn fun_id in
  if fun_name = "execute" then (
    let (Pat_aux (pat, _)) = func in
    let args, body =
      match pat with Pat_exp (a, b) -> (a, b) | Pat_when (a, b, _) -> (a, b)
    in
    let case_name, arg_names = destructure_union_arglist args in
    let reg_operands =
      ["regidx"; "cregidx"; "fregidx"; "vregidx"]
      |> List.map (infer_registerfile_for_typename case_name arg_names)
      |> List.flatten
    in
    if Hashtbl.mem state.op_info.registers_info case_name then (
      let existing = Hashtbl.find state.op_info.registers_info case_name in
      if not (are_operand_lists_equal existing reg_operands) then
        failwith
          ("AST case " ^ case_name
         ^ " has different operand types for different specializations of the \
            execute function"
          )
    )
    else Hashtbl.add state.op_info.registers_info case_name reg_operands
  )

let get_arg_idx arg_id args =
  let arg_name = id_to_str arg_id in
  let arg_idx = ref (-1) in
  List.iteri
    (fun idx a ->
      match a with
      | MP_aux (MP_id i, _) when id_to_str i = arg_name -> arg_idx := idx
      | _ -> ()
    )
    args;
  if !arg_idx <> -1 then Some !arg_idx else None

let infer_immediate_in_pattern p args =
  let arg_index_from_arg_pattern arg =
    match arg with
    | MP_aux (MP_id i, _) -> get_arg_idx i args
    | MP_aux (MP_vector_concat pats, _) ->
        let indices =
          List.filter_map
            (fun p ->
              match p with
              | MP_aux (MP_id i, _) -> get_arg_idx i args
              | MP_aux (MP_typ (MP_aux (MP_id i, _), _), _) ->
                  get_arg_idx i args
              | _ -> None
            )
            pats
        in
        assert_empty_or_length1_or_failwith indices
          "Expected a vector concatentation expresion to have at most 1 id";
        get_sole_element_or_none indices
    | _ -> None
  in
  match p with
  | MP_aux (MP_app (fun_id, [arg]), _)
    when str_starts_with "hex_bits" (id_to_str fun_id) ->
      arg_index_from_arg_pattern arg
  | _ -> None

let infer_immediates state _ id _ left right =
  if id_to_str id = "assembly" then (
    match left with
    | MPat_aux (MPat_pat pl, _) | MPat_aux (MPat_when (pl, _), _) -> (
        match pl with
        | MP_aux (MP_app (case_id, args), _) -> (
            match right with
            | MPat_aux (MPat_pat pr, _) | MPat_aux (MPat_when (pr, _), _) -> (
                match pr with
                | MP_aux (MP_string_append patterns, _) ->
                    let imm_indices =
                      List.filter_map
                        (fun p -> infer_immediate_in_pattern p args)
                        patterns
                    in
                    if List.length imm_indices <> 0 then (
                      let imms =
                        List.map (fun index -> Imm index) imm_indices
                      in
                      let case_name = id_to_str case_id in
                      if Hashtbl.mem state.op_info.immediates_info case_name
                      then (
                        let existing =
                          Hashtbl.find state.op_info.immediates_info case_name
                        in
                        let mismatch =
                          if List.length imms <> List.length existing then true
                          else
                            List.combine imms existing
                            |> List.map (fun (Imm i1, Imm i2) -> i1 = i2)
                            |> List.for_all (fun eq -> eq)
                        in
                        if mismatch then
                          failwith
                            ("AST case " ^ case_name
                           ^ " has different immediate operands depending on \
                              its specialization"
                            )
                      )
                      else
                        Hashtbl.add state.op_info.immediates_info case_name imms
                    )
                | _ -> ()
              )
          )
        | _ -> ()
      )
  )

let gen_operand_info ast analysis =
  let processor =
    {
      default_processor with
      process_function_clause = infer_registers;
      process_mapping_bidir_clause = infer_immediates;
    }
  in
  let state =
    {
      analysis;
      op_info =
        {
          registers_info = Hashtbl.create 200;
          immediates_info = Hashtbl.create 200;
        };
    }
  in
  foreach_node ast processor state;
  state.op_info
