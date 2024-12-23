open Assembler
open Clike_typedef
open Sail_values
open Decoder
open Decode_procedure
open Constants
open Gen_clike_typedef
open Sail_utils
open Utils
open Hashset

let stringify_clike_builin name builtin =
  match builtin with
  | Clike_bitfield size ->
      let pow2_rounded_size =
        if size < 8 then 8
        else if size < 16 then 16
        else if size < 32 then 32
        else 64
      in
      "uint"
      ^ string_of_int pow2_rounded_size
      ^ "_t " ^ name ^ " /* bits : " ^ string_of_int size ^ " */;"
  | Clike_byte -> "uint8_t " ^ name ^ ";"
  | Clike_word -> "uint16_t " ^ name ^ ";"
  | Clike_dword -> "uint32_t " ^ name ^ ";"
  | Clike_qword -> "uint64_t " ^ name ^ ";"

let rec stringify_clike_typedef clike_typdef =
  match clike_typdef with
  | Clike_enum (typname, name, constants) ->
      "enum " ^ typname ^ " {" ^ String.concat "," constants ^ "} " ^ name ^ ";"
  | Clike_struct (typname, name, members) ->
      let members_as_str = List.map stringify_clike_typedef members in
      "struct " ^ typname ^ " {"
      ^ String.concat "" members_as_str
      ^ "} " ^ name ^ ";"
  | Clike_union (typname, name, members) ->
      let members_as_str = List.map stringify_clike_typedef members in
      "union " ^ typname ^ " {"
      ^ String.concat "" members_as_str
      ^ "} " ^ name ^ ";"
  | Clike_builtin (name, bitvec) -> stringify_clike_builin name bitvec
  | Clike_void -> ""
  | Clike_typename (typname, name) -> typname ^ " " ^ name ^ ";"

let stringify_typdef typdef =
  "enum {" ^ identifier_prefix ^ "false = 0, " ^ identifier_prefix
  ^ "true = 1}; "
  ^ stringify_clike_typedef typdef

type decproc_stringification_state = {
  typedef_walker : typedef_walker;
  currently_defined_bv_sizes : (string, int) Hashtbl.t;
}

let rec bv_len vars bv =
  match bv with
  | Literal lit -> (String.length lit - 2) * 4
  | Binstr_slice (i1, i2) -> i2 - i1
  | Concat bvs -> List.fold_left ( + ) 0 (List.map (bv_len vars) bvs)
  | Id name -> Hashtbl.find vars.currently_defined_bv_sizes name

let rec stringify_bv vars e =
  match e with
  | Literal l -> l
  | Binstr_slice (i1, i2) ->
      let mask =
        0 :: List.init 63 (fun x -> x + 1)
        |> List.map (fun i -> if i >= i1 && i < i2 then "1" else "0")
        |> List.rev |> String.concat "" |> binary_str_to_hex_str
      in
      let masking_expr = binary_stream_c_parameter ^ " & 0x" ^ mask in
      if i1 <> 0 then "(" ^ masking_expr ^ ")" ^ ">>" ^ string_of_int i1
      else masking_expr
  | Concat bvs ->
      let offsets =
        List.rev
          (List.fold_left
             (fun lengths bv ->
               match lengths with
               | length_prev :: rest ->
                   let length = bv_len vars bv in
                   (length + length_prev) :: lengths
               | _ -> failwith "UNREACHABLE"
             )
             [0] bvs
          )
      in
      let shifted_exprs =
        List.mapi
          (fun i bv ->
            stringify_bv vars bv ^ " << " ^ string_of_int (List.nth offsets i)
          )
          bvs
      in
      "(" ^ String.concat ") | (" (List.rev shifted_exprs) ^ ")"
  | Id name -> name

let rec stringify_bool vars b =
  match b with
  | Is_eq (bv_expr, valu) -> "(" ^ stringify_bv vars bv_expr ^ ") == " ^ valu
  | Is_enum_var_valid var -> var ^ " != 0xFFFF" ^ "FFFF" ^ "FFFF" ^ "FFFF"
  | Is_struct_var_valid var -> var ^ "_is_valid == 1"
  | And exprs ->
      "(" ^ String.concat ") && (" (List.map (stringify_bool vars) exprs) ^ ")"

let rec stringify_stmt str_state stmt =
  match stmt with
  | Start_rule rule_name ->
      "\n// ---------------------------" ^ rule_name
      ^ "-------------------------------\n {"
  | End_rule ->
      "}\n//------------------------------------------------------------\n"
  | Init (var, expr) ->
      Hashtbl.add str_state.currently_defined_bv_sizes var
        (bv_len str_state expr);
      "uint64_t " ^ var ^ " = " ^ stringify_bv str_state expr ^ " ;"
  | If (cond, body) ->
      "if ("
      ^ stringify_bool str_state cond
      ^ ") { "
      ^ stringify_stmt str_state body
      ^ "}"
  | Switch_assign (var, exp, cases) ->
      let var_decl =
        "uint64_t " ^ var ^ " = 0xFFFF" ^ "FFFF" ^ "FFFF" ^ "FFFF ;"
      in
      let switch_start = "switch (" ^ stringify_bv str_state exp ^ ") {" in
      let cases =
        List.map
          (fun (bval, enmval) ->
            "case " ^ bval ^ ": " ^ var ^ " = "
            ^ add_prefix_unless_exists identifier_prefix enmval
            ^ " ;" ^ "break; "
          )
          cases
      in
      var_decl ^ switch_start ^ String.concat "" cases ^ "}"
  | Switch_assign_struct (typ, var, exp, cases) ->
      let var_decl =
        "struct " ^ typ ^ " " ^ var ^ " ;" ^ "uint8_t " ^ var
        ^ "_is_valid = 0 ;"
      in
      let switch_start = "switch (" ^ stringify_bv str_state exp ^ ") { " in
      let c_cases =
        cases
        |> List.map (fun (bval, kvs) ->
               let member_assignments =
                 kvs
                 |> List.map (fun (k, v) ->
                        var ^ "." ^ k ^ " = "
                        ^ ( match v with
                          | Bool_const c -> if c then "1" else "0"
                          | Bv_const s | Binding s -> s
                          | Enum_lit s ->
                              add_prefix_unless_exists identifier_prefix s
                          )
                        ^ ";"
                    )
               in
               "case " ^ bval ^ ": "
               ^ String.concat "" member_assignments
               ^ var ^ "_is_valid = 1 ; " ^ "break; "
           )
      in
      var_decl ^ switch_start ^ String.concat "" c_cases ^ "}"
  | Set_ast_case case ->
      let case_setter_path = set_walker_case str_state.typedef_walker case in
      ast_c_parameter ^ "->" ^ case_setter_path ^ " = "
      ^ add_prefix_unless_exists identifier_prefix case
      ^ " ;"
  | Set_ast_next_case_member member_rhs -> (
      let rhs_string =
        match member_rhs with
        | Val v -> (
            match v with
            | Bool_const c -> if c then "1" else "0"
            | Bv_const s | Binding s -> s
            | Enum_lit s -> add_prefix_unless_exists identifier_prefix s
          )
        | Exp bv_expr -> stringify_bv str_state bv_expr
      in
      match walk str_state.typedef_walker with
      | Some setter_path ->
          ast_c_parameter ^ "->" ^ setter_path ^ " = " ^ rhs_string ^ ";"
      | None -> failwith "Error assigning to an ast member"
    )
  | Ret_ast -> "return " ^ ";"
  | Block stmts -> String.concat "" (List.map (stringify_stmt str_state) stmts)

let stringify_decode_procedure ?(c_proc_name = "decode") (Proc stmt) walker =
  let procedure_start =
    "void " ^ c_proc_name ^ "(struct " ^ ast_sail_def_name ^ " *"
    ^ ast_c_parameter ^ ", uint64_t " ^ binary_stream_c_parameter ^ ") {"
  in
  let procedure_end = "}" in
  let initial_state =
    { typedef_walker = walker; currently_defined_bv_sizes = Hashtbl.create 100 }
  in
  let procedure_body = stringify_stmt initial_state stmt in
  procedure_start ^ procedure_body ^ procedure_end

type assembler_stringification_state = {
  walker : typedef_walker;
  already_defined_tables : string set;
}

let stringify_tostr_tbl { walker; already_defined_tables } name idx arg_idx tbl
    =
  let tbl_call =
    name ^ "(" ^ ast_c_parameter ^ "->"
    ^ Option.get (get_member_path walker arg_idx)
    ^ ", ss);"
  in
  let tbl_def =
    if not (set_contains already_defined_tables name) then (
      let proc_start = "void " ^ name ^ "(uint64_t member, SStream *ss) {" in
      let cases = ref [] in
      Hashtbl.iter
        (fun k v ->
          let case =
            "case "
            ^ ( if k.[0] = '0' then k
                else add_prefix_unless_exists identifier_prefix k
              )
            ^ ":{" ^ "SStream_concat(ss, \"" ^ v ^ "\"); break; }"
          in
          cases := case :: !cases
        )
        tbl;
      set_add already_defined_tables name;
      proc_start ^ "switch (member) { " ^ String.concat "" !cases ^ "}}"
    )
    else ""
  in
  (tbl_call, tbl_def)

let stringify_intrinsic_logic_arg walker arg =
  match arg with
  | Arg_index idx -> "tree->" ^ Option.get (get_member_path walker idx)
  | Bv_concat bv_vals ->
      let bv_sizes =
        List.map
          (fun bv_val ->
            match bv_val with Arg_idx (_, len) | Bv_lit (_, len) -> len
          )
          bv_vals
      in
      let cumulative_sizes =
        List.fold_right
          (fun sz cumulative_sizes ->
            match cumulative_sizes with
            | s :: rest -> (s + sz) :: cumulative_sizes
            | _ -> failwith "UNREACHABLE"
          )
          bv_sizes [0]
      in
      let shifted_args =
        List.mapi
          (fun i bv_val ->
            let shift =
              " << " ^ string_of_int (List.nth cumulative_sizes (i + 1))
            in
            match bv_val with
            | Arg_idx (idx, _) ->
                "tree->" ^ Option.get (get_member_path walker idx) ^ shift
            | Bv_lit (lit_str, _) -> lit_str ^ shift
          )
          bv_vals
      in
      String.concat " | " shifted_args

let stringfy_tostr_logic ({ walker; _ } as str_state) i tostr =
  match tostr with
  | Lit s -> ("SStream_concat(ss, \"" ^ s ^ "\");", "")
  | Bitv2Str (name, arg_idx, tbl) ->
      stringify_tostr_tbl str_state name i arg_idx tbl
  | Enum2Str (name, arg_idx, tbl) ->
      stringify_tostr_tbl str_state name i arg_idx tbl
  | Bool2Str (_, arg_idx, (false_case, true_case)) ->
      let true_conc = "SStream_concat(ss, \"" ^ true_case ^ "\");" in
      let false_conc = "SStream_concat(ss, \"" ^ false_case ^ "\");" in
      ( "if (" ^ ast_c_parameter ^ "->"
        ^ Option.get (get_member_path walker arg_idx)
        ^ ") {" ^ true_conc ^ "}" ^ "else {" ^ false_conc ^ "} ",
        ""
      )
  | Struct2str (_, arg_idx, tbl) ->
      let struct_arg =
        ast_c_parameter ^ "->" ^ Option.get (get_member_path walker arg_idx)
      in
      let cases = ref [] in
      Hashtbl.iter
        (fun kv_pairs string ->
          let cond =
            List.map
              (fun (key, valu) ->
                "(" ^ struct_arg ^ "." ^ key ^ " == "
                ^ ( match valu with
                  | Bv_const s -> s
                  | Bool_const b -> if b then "1" else "0"
                  | Binding s -> s
                  | Enum_lit e -> add_prefix_unless_exists identifier_prefix e
                  )
                ^ ")"
              )
              kv_pairs
          in
          let cond = String.concat " && " cond in
          let case =
            "if (" ^ cond ^ ") { SStream_concat(ss, \"" ^ string ^ "\");} else "
          in
          cases := case :: !cases
        )
        tbl;

      (String.concat "" !cases ^ ";", "")
  | Intrinsic_tostr_logic (name, args) ->
      let sep = if List.length args != 0 then "," else "" in
      let args = List.map (stringify_intrinsic_logic_arg walker) args in
      (name ^ "(" ^ String.concat ", " args ^ sep ^ "ss, conf);", "")

let stringify_subcase_body ({ walker; _ } as str_state) body =
  let tostrs_and_tables = List.mapi (stringfy_tostr_logic str_state) body in
  let stringified_tostr = String.concat "" (List.map fst tostrs_and_tables) in
  let tables = String.concat "" (List.map snd tostrs_and_tables) in

  (stringified_tostr, tables)

let stringify_clause_subcase ({ walker; _ } as str_state)
    (subcase_condition, subcase_body) =
  let subcase_body, tables = stringify_subcase_body str_state subcase_body in
  let subcase =
    match subcase_condition with
    | None -> subcase_body
    | Some (arg_idx, enum_value) ->
        "if (" ^ ast_c_parameter ^ "->"
        ^ Option.get (get_member_path walker arg_idx)
        ^ " == "
        ^ add_prefix_unless_exists identifier_prefix enum_value
        ^ ") { " ^ subcase_body ^ "}"
  in
  (subcase, tables)

let stringify_assembler_clause ({ walker; _ } as str_state) (case_name, subcases)
    =
  set_walker_case walker case_name;
  let subconds_and_tables =
    List.map (stringify_clause_subcase str_state) subcases
  in
  let subconds = List.map fst subconds_and_tables in
  let tables = List.map snd subconds_and_tables in
  let clause =
    "case "
    ^ add_prefix_unless_exists identifier_prefix case_name
    ^ ": " ^ String.concat "" subconds ^ "break;"
  in
  (clause, String.concat "" tables)

let stringify_assembler asm walker =
  let procedure_start =
    "static void ast2str(struct " ^ ast_sail_def_name ^ " *" ^ ast_c_parameter
    ^ ", SStream *ss, riscv_conf *conf) { " ^ "switch (" ^ ast_c_parameter
    ^ "->" ^ ast_sail_def_name ^ generated_ast_enum_suffix ^ ") {"
  in
  let procedure_end = "}}" in
  let initial_state = { walker; already_defined_tables = Hashtbl.create 100 } in
  let body_and_tables =
    List.map (stringify_assembler_clause initial_state) asm
  in
  let procedure_body = String.concat "" (List.map fst body_and_tables) in
  let tables = String.concat "" (List.map snd body_and_tables) in
  (procedure_start ^ procedure_body ^ procedure_end, tables)

let stringify_instr_types instr_types typedef_walker =
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
    (fun case_name (i, types) ->
      set_walker_case typedef_walker case_name;
      let case_name = add_prefix_unless_exists identifier_prefix case_name in
      put "\n//--------------------- ";
      put case_name;
      put "--------------------- \n";

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
           (List.map (fun t -> "RISCV_INSN_" ^ (strip_prefix_if_exists identifier_prefix t) ) types)
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
