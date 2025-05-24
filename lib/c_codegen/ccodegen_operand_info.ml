open Either

open Capstone_autosync_sail

open Constants
open Utils
open Gen_clike_typedef
open Gen_operand_info_defs

let single_operand_to_c index operand walker =
  match operand with
  | Left (Reg (idx, regfile, regacess)) ->
      let regfile_index_typecaster =
        match regfile with
        | Base -> "AS_GEN_PURPOSE_REG"
        | Float_or_Double -> "AS_FLOAT_REG"
        (* assume the compressed register is a general purpose register, true for most cases *)
        (* float cases will be patched manually later *)
        | Base_or_Float -> "AS_COMPRESSED_GEN_PURPOSE_REG"
        | Vector -> "AS_VECTOR_REG"
      in
      let op_indexing = "ops[" ^ string_of_int index ^ "]" in
      op_indexing ^ ".type = RISCV_OP_REG;" ^ op_indexing ^ ".reg = "
      ^ regfile_index_typecaster ^ "(" ^ ast_c_parameter ^ "->"
      ^ Option.get (get_member_path walker idx)
      ^ ");" ^ op_indexing ^ ".access = "
      ^ ( if regacess = Read then "CS_AC_READ"
          else if regacess == Write then "CS_AC_WRITE"
          else "CS_AC_READ | CS_AC_WRITE"
        )
      ^ ";"
  | Right (Imm idx) ->
      let op_indexing = "ops[" ^ string_of_int index ^ "]" in
      op_indexing ^ ".type = RISCV_OP_IMM;" ^ op_indexing ^ " .imm = "
      ^ ast_c_parameter ^ "->"
      ^ Option.get (get_member_path walker idx)
      ^ ";" ^ op_indexing ^ " .access = CS_AC_READ;"

let operand_lists_to_c reg_ops imm_ops walker =
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
    List.mapi (fun i op -> single_operand_to_c i op walker) sorted_operands
  in
  ("*op_count = " ^ string_of_int (List.length statemets) ^ ";") :: statemets

let case_operand_info_to_c name reg_ops imm_ops walker =
  set_walker_case walker name;

  let case_start =
    "case " ^ add_prefix_unless_exists identifier_prefix name ^ ": {"
  in
  let statements = operand_lists_to_c reg_ops imm_ops walker in
  case_start ^ String.concat "" statements ^ "break;}"

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
