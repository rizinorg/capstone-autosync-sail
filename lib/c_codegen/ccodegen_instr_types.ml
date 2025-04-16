open Capstone_autosync_sail

open Constants
open Gen_clike_typedef
open Utils

open Gen_instr_types

let instr_types_to_c instr_types typedef_walker =
  let enum_def = Buffer.create 10000 in
  let mapping = Buffer.create 10000 in
  let mapping_table = Buffer.create 10000 in

  let put = Buffer.add_string enum_def in
  let apnd = Buffer.add_string mapping in
  let prnt = Buffer.add_string mapping_table in

  put "enum ";
  put (String.lowercase_ascii identifier_prefix);
  put "insn { ";

  prnt " = {";

  apnd "uint16_t get_insn_type(";
  apnd ("struct " ^ ast_sail_def_name ^ " *" ^ ast_c_parameter);
  apnd ") { switch (";
  apnd (ast_c_parameter ^ "->" ^ ast_sail_def_name ^ generated_ast_enum_suffix);
  apnd ") {";

  let max_num_instr_types = ref 0 in
  Hashtbl.iter
    (fun case_name i_types ->
      set_walker_case typedef_walker case_name;
      let case_name = add_prefix_unless_exists identifier_prefix case_name in
      put "\n//--------------------- ";
      put case_name;
      put "--------------------- \n";

      let i, types, should_define_enum_cases =
        match i_types with
        | Types (i, types) -> (i, types, true)
        | Same_as another_case -> (
            match Hashtbl.find instr_types another_case with
            | Types (i, types) -> (i, types, false)
            | _ -> failwith "UNREACHABLE"
          )
      in
      if should_define_enum_cases then
        List.iter
          (fun typename ->
            put "RISCV_INSN_";
            put (strip_prefix_if_exists identifier_prefix typename);
            put ","
          )
          types;
      let len = List.length types in
      if len > !max_num_instr_types then max_num_instr_types := len;

      prnt ("[" ^ case_name ^ "] = {");
      prnt
        (String.concat ","
           (List.map
              (fun t ->
                "RISCV_INSN_" ^ strip_prefix_if_exists identifier_prefix t
              )
              types
           )
        );
      prnt "},";

      if i != -1 then (
        apnd "case ";
        apnd case_name;
        apnd ": return to_insn[";
        apnd case_name;
        apnd "][";
        apnd (ast_c_parameter ^ "->");
        apnd (Option.get (get_member_path typedef_walker i));
        apnd "];"
      )
    )
    instr_types;
  (* Close the enum definition *)
  put "};";
  (* Close the function definition *)
  apnd "default: return to_insn[";
  apnd (ast_c_parameter ^ "->" ^ ast_sail_def_name ^ generated_ast_enum_suffix);
  apnd "][0];}}";
  (* Close the table definition *)
  prnt "};";
  let table_decl =
    "static const uint16_t to_insn["
    ^ string_of_int (Hashtbl.length instr_types)
    ^ "]["
    ^ string_of_int !max_num_instr_types
    ^ "]"
  in
  ( Buffer.contents enum_def,
    table_decl ^ Buffer.contents mapping_table ^ Buffer.contents mapping
  )
