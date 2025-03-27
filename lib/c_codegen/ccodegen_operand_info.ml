open Either

open Capstone_autosync_sail

open Constants
open Utils
open Gen_clike_typedef
open Gen_operand_info_defs

let single_operand_info_to_c index operand walker =
  match operand with
  | Left (Reg (idx, regfile, regacess)) ->
      let regfile_index_typecaster =
        match regfile with
        | Base -> "AS_GEN_PURPOSE_REG"
        | Float -> "AS_FLOAT_REG"
        | Double -> "AS_DOUBLE_REG"
        | Vector -> "AS_VECTOR_REG"
      in
      let op_indexing = "ops[" ^ string_of_int index ^ "]" in
      op_indexing ^ ".type = RISCV_OP_REG;" ^ op_indexing ^ ".reg = "
      ^ regfile_index_typecaster ^ "(" ^ ast_c_parameter ^ "->"
      ^ Option.get (get_member_path walker idx)
      ^ ");" ^ op_indexing ^ ".access = "
      ^ (if regacess = Read then "CS_AC_READ" else "CS_AC_WRITE")
      ^ ";"
  | Right (Imm idx) ->
      "ops[" ^ string_of_int index ^ "] = { " ^ ".type = RISCV_OP_IMM, .imm = "
      ^ ast_c_parameter ^ "->"
      ^ Option.get (get_member_path walker idx)
      ^ ", .access = CS_AC_READ };"

let subcase_operands_info_to_c reg_ops imm_ops walker =
  let compare_ops op1 op2 =
    match (op1, op2) with
    | Left (Reg (i1, _, _)), Right (Imm i2)
    | Right (Imm i1), Left (Reg (i2, _, _))
    | Right (Imm i1), Right (Imm i2)
    | Left (Reg (i1, _, _)), Left (Reg (i2, _, _)) ->
        Int.compare i1 i2
  in
  let some_operands = List.map (fun reg -> Left reg) reg_ops in
  let apnd = List.append some_operands in
  let all_operands = apnd (List.map (fun imm -> Right imm) imm_ops) in
  let sorted_operands = List.sort compare_ops all_operands in
  let statemets =
    List.mapi (fun i op -> single_operand_info_to_c i op walker) sorted_operands
  in
  statemets

let case_operand_info_to_c name reg_ops imm_ops walker =
  let get_at_most_1_specialization ops =
    let indices =
      ops
      |> List.filter_map (fun (s, _) ->
             if Option.is_none s then None else Some (fst (Option.get s))
         )
      |> List.sort_uniq Int.compare
    in
    let len = List.length indices in
    if len > 1 then failwith "Multiple arguments specialized, Unsupported"
    else if len = 0 then -1
    else List.nth indices 0
  in
  let specialization_to_c s body =
    let case_start =
      match s with
      | None -> "default: {"
      | Some (_, value) ->
          "case " ^ add_prefix_unless_exists identifier_prefix value ^ ": {"
    in
    case_start ^ body ^ "break;}"
  in
  set_walker_case walker name;

  let case_start =
    "case " ^ add_prefix_unless_exists identifier_prefix name ^ ": {"
  in
  match (reg_ops, imm_ops) with
  | [], [] -> ""
  | [(None, r_ops)], [] ->
      let statements = subcase_operands_info_to_c r_ops [] walker in
      case_start ^ String.concat "" statements ^ "break;}"
  | [], [(None, i_ops)] ->
      let statements = subcase_operands_info_to_c [] i_ops walker in
      case_start ^ String.concat "" statements ^ "break;}"
  | [(None, r_ops)], [(None, i_ops)] ->
      let statements = subcase_operands_info_to_c r_ops i_ops walker in
      case_start ^ String.concat "" statements ^ "break;}"
  | _ ->
      let r_specialization_idx = get_at_most_1_specialization reg_ops in
      let i_specialization_idx = get_at_most_1_specialization imm_ops in
      assert (i_specialization_idx <> -1 || r_specialization_idx <> -1);

      let statements =
        if r_specialization_idx = -1 then (
          let switch_start =
            "switch (" ^ ast_c_parameter ^ "->"
            ^ Option.get (get_member_path walker i_specialization_idx)
            ^ ") {"
          in
          let _, r_ops =
            Option.value (get_sole_element_or_none reg_ops) ~default:(None, [])
          in
          let cases =
            imm_ops
            |> List.map (fun (s, ops) ->
                   let body =
                     String.concat ""
                       (subcase_operands_info_to_c r_ops ops walker)
                   in
                   specialization_to_c s body
               )
          in
          switch_start ^ String.concat "" cases ^ "}"
        )
        else if i_specialization_idx = -1 then (
          let switch_start =
            "switch (" ^ ast_c_parameter ^ "->"
            ^ Option.get (get_member_path walker r_specialization_idx)
            ^ ") {"
          in
          let _, i_ops =
            Option.value (get_sole_element_or_none imm_ops) ~default:(None, [])
          in
          let cases =
            reg_ops
            |> List.map (fun (s, ops) ->
                   let body =
                     String.concat ""
                       (subcase_operands_info_to_c ops i_ops walker)
                   in
                   specialization_to_c s body
               )
          in
          switch_start ^ String.concat "" cases ^ "}"
        )
        else failwith "UNREACHABLE:"
      in
      case_start ^ statements ^ "break;}"

let operand_info_to_c op_info walker =
  let procedure_start =
    "static void fill_operands(struct " ^ ast_sail_def_name ^ " *"
    ^ ast_c_parameter ^ ", cs_riscv_op *ops, uint8_t *op_count) {"
  in
  let procedure_start =
    procedure_start ^ "switch (" ^ ast_c_parameter ^ "->" ^ ast_sail_def_name
    ^ generated_ast_enum_suffix ^ ") {"
  in
  let cases = ref [] in
  Hashtbl.iter
    (fun case_name reg_operands ->
      let imm_operands =
        if Hashtbl.mem op_info.immediates_info case_name then
          Hashtbl.find op_info.immediates_info case_name
        else []
      in
      cases :=
        case_operand_info_to_c case_name reg_operands imm_operands walker
        :: !cases
    )
    op_info.registers_info;
  procedure_start ^ String.concat "" !cases ^ " }}"
