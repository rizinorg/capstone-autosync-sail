open Capstone_autosync_sail

open Sail_values
open Decoder
open Constants
open Gen_clike_typedef
open Utils

type decproc_stringification_state = {
  typedef_walker : typedef_walker;
  currently_defined_bv_sizes : (string, int) Hashtbl.t;
}

let gen_c_consequences state consequences =
  let gen_c_single_consequence_item conseq =
    let member_path = Option.get (walk state.typedef_walker) in
    let member_rhs =
      match conseq with
      | Push v -> (
          match v with
          | Bool_const c -> if c then "1" else "0"
          | Bv_const s | Binding s -> s
          | Enum_lit s -> add_prefix_unless_exists identifier_prefix s
        )
      | Concat_push vs ->
          let result = ref [] in
          let len_consumed = ref 0 in
          List.iter
            (fun v ->
              let shift =
                if !len_consumed = 0 then "" else string_of_int !len_consumed
              in
              let e =
                match v with
                | Bv_const c ->
                    len_consumed := !len_consumed + ((String.length c - 2) * 4);
                    c
                | Binding s ->
                    len_consumed :=
                      !len_consumed
                      + Hashtbl.find state.currently_defined_bv_sizes s;
                    s
                | _ -> failwith "Can't concat non-bitvec values"
              in
              if shift <> "" then
                result := ("(" ^ e ^ "<<" ^ shift ^ ")") :: !result
              else result := e :: !result
            )
            vs;
          String.concat "|" !result
    in
    ast_c_parameter ^ "->" ^ member_path ^ "=" ^ member_rhs
  in
  let Assign_node_type case, items = consequences in
  let case_setter_path = set_walker_case state.typedef_walker case in
  let case_set_stmt =
    ast_c_parameter ^ "->" ^ case_setter_path ^ " = "
    ^ add_prefix_unless_exists identifier_prefix case
    ^ " ;"
  in
  let items_set_stmt =
    String.concat ";" (List.map gen_c_single_consequence_item items)
  in
  case_set_stmt ^ items_set_stmt ^ ";return;"

let annotate_conds_with_start_offsets conditions =
  let result = ref [] in
  let len_consumed_so_far = ref 0 in
  List.iter
    (fun cond ->
      match cond with
      | Assert (len, _)
      | Bind (len, _)
      | Map_bind (len, _, _)
      | Struct_map_bind (len, _, _, _) ->
          result := (!len_consumed_so_far, cond) :: !result;
          len_consumed_so_far := !len_consumed_so_far + len
    )
    conditions;
  List.rev !result

let gen_c_assert (start_offset, condition) =
  match condition with
  | Assert (len, value) ->
      let end_offset = start_offset + len - 1 in
      Some
        ("SLICE_BITVEC(" ^ binary_stream_c_parameter ^ " , "
       ^ string_of_int start_offset ^ " , " ^ string_of_int end_offset ^ ") =="
       ^ value
        )
  | _ -> None

let gen_c_bind state (start_offset, condition) =
  match condition with
  | Bind (len, var) ->
      Hashtbl.add state.currently_defined_bv_sizes var len;
      let end_offset = start_offset + len - 1 in
      Some
        ("uint64_t " ^ var ^ " = SLICE_BITVEC(" ^ binary_stream_c_parameter
       ^ " , " ^ string_of_int start_offset ^ " , " ^ string_of_int end_offset
       ^ ");"
        )
  | _ -> None

let gen_c_mapbind (start_offset, condition) =
  match condition with
  | Map_bind (len, bv2enum_table, var) ->
      let end_offset = start_offset + len - 1 in
      let init =
        "uint64_t " ^ var ^ " = 0x" ^ "FFFF" ^ "FFFF" ^ "FFFF" ^ "FFFF ;"
      in
      let switch_start =
        "switch (SLICE_BITVEC(" ^ binary_stream_c_parameter ^ " , "
        ^ string_of_int start_offset ^ " , " ^ string_of_int end_offset
        ^ ")) { "
      in
      let c_cases = ref [
        "default: break;";
      ] in
      Hashtbl.iter
        (fun bval enumval ->
          let c_case =
            "case " ^ bval ^ ": " ^ var ^ " = "
            ^ add_prefix_unless_exists identifier_prefix enumval
            ^ ";break; "
          in
          c_cases := c_case :: !c_cases
        )
        bv2enum_table;
      Some
        (init ^ switch_start ^ String.concat "" !c_cases ^ "} if (" ^ var
       ^ "!= 0x" ^ "FFFF" ^ "FFFF" ^ "FFFF" ^ "FFFF) {"
        )
  | Struct_map_bind (len, struct_typename, bv2struct_table, var) ->
      let end_offset = start_offset + len - 1 in
      let var_decl =
        "struct " ^ struct_typename ^ " " ^ var ^ " ;" ^ "uint8_t " ^ var
        ^ "_is_valid = 0 ;"
      in
      let switch_start =
        "switch (SLICE_BITVEC(" ^ binary_stream_c_parameter ^ " , "
        ^ string_of_int start_offset ^ " , " ^ string_of_int end_offset
        ^ ")) { "
      in
      let c_cases = ref [
        "default: break;";
      ] in
      Hashtbl.iter
        (fun bval kv_pairs ->
          let member_assignments =
            kv_pairs
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
          let c_case =
            "case " ^ bval ^ ": "
            ^ String.concat "" member_assignments
            ^ var ^ "_is_valid = 1 ; " ^ "break; "
          in
          c_cases := c_case :: !c_cases
        )
        bv2struct_table;
      Some
        (var_decl ^ switch_start ^ String.concat "" !c_cases ^ "} if (" ^ var
       ^ "_is_valid == 1) {"
        )
  | _ -> None

let rec gen_c_guards cond bound_ids currently_defined_bv_sizes =
  let gen_c_int_operand op =
    match op with
    | Id_or_funcall (name, []) ->
        if List.mem name bound_ids then name else "ctx->" ^ name
    | Id_or_funcall (name, args) ->
        name ^ "(" ^ String.concat "," args ^ ", ctx)"
    | Number n -> string_of_int n
  in
  match cond with
  | Eq_int (op1, op2) -> gen_c_int_operand op1 ^ " == " ^ gen_c_int_operand op2
  | Less_eq_int (op1, op2) ->
      gen_c_int_operand op1 ^ " < " ^ gen_c_int_operand op2
  | Less_eq_or_eq (op1, op2) ->
      gen_c_int_operand op1 ^ " <= " ^ gen_c_int_operand op2
  | Eq_bit (bv, idx, bit) ->
      "INDEX_BITVEC(" ^ bv ^ "," ^ string_of_int idx ^ ") == "
      ^ if bit then "1" else "0"
  | Eq_bv (left, right) -> (
      let left =
        List.map
          (fun l ->
            if List.mem l bound_ids || l.[0] == '0' then l else "ctx->" ^ l
          )
          left
      in
      let right =
        if List.mem right bound_ids || right.[0] == '0' then right
        else "ctx->" ^ right
      in
      match left with
      | [bv] -> bv ^ " == " ^ right
      | bv_values ->
          let len_consumed = ref 0 in
          let result = ref [] in
          List.iter
            (fun valu ->
              let shift =
                if !len_consumed = 0 then "" else string_of_int !len_consumed
              in
              let e =
                match valu.[0] with
                | '0' ->
                    let val_len = (String.length valu - 2) * 4 in
                    len_consumed := !len_consumed + val_len;
                    valu
                | identifier ->
                    let val_len =
                      Hashtbl.find currently_defined_bv_sizes valu
                    in
                    len_consumed := !len_consumed + val_len;
                    valu
              in
              if shift <> "" then
                result := ("(" ^ e ^ "<<" ^ shift ^ ")") :: !result
              else result := e :: !result
            )
            bv_values;
          "(" ^ String.concat "|" !result ^ ") == " ^ right
    )
  | Less_eq_bv (left, right) -> left ^ " < " ^ right
  | Boolfun (name, args) ->
      let argnames =
        List.map
          (fun n ->
            match n.[0] with 'A' .. 'Z' -> identifier_prefix ^ n | _ -> n
          )
          args
      in
      let sep = if List.length args > 0 then "," else "" in
      name ^ "(" ^ String.concat "," argnames ^ sep ^ "ctx" ^ ")"
  | And (g1, g2) ->
      "("
      ^ gen_c_guards g1 bound_ids currently_defined_bv_sizes
      ^ ") && ("
      ^ gen_c_guards g2 bound_ids currently_defined_bv_sizes
      ^ ")"
  | Or (g1, g2) ->
      "("
      ^ gen_c_guards g1 bound_ids currently_defined_bv_sizes
      ^ ") || ("
      ^ gen_c_guards g2 bound_ids currently_defined_bv_sizes
      ^ ")"
  | Not g -> "!(" ^ gen_c_guards g bound_ids currently_defined_bv_sizes ^ ")"
  | True -> ""

let get_all_bound_identifiers conditions =
  let get_bound_id cond =
    match cond with
    | Assert _ -> None
    | Bind (_, id) -> Some id
    | Map_bind (_, _, id) -> Some id
    | Struct_map_bind (_, _, _, id) -> Some id
  in
  List.filter_map get_bound_id conditions

let rec nest mapbinds guards binds conseqs =
  match mapbinds with
  | [] -> (
      match guards with
      | "" -> String.concat "" binds ^ conseqs
      | conditions ->
          String.concat "" binds ^ "if (" ^ conditions ^ ") {" ^ conseqs ^ "}"
    )
  | mb :: rest -> mb ^ nest rest guards binds conseqs ^ "}"

let gen_c_rule state rule =
  Hashtbl.clear state.currently_defined_bv_sizes;

  let conditions, guards, consequences = rule in
  let conditions_with_offsets = annotate_conds_with_start_offsets conditions in
  let assert_c_exprs = List.filter_map gen_c_assert conditions_with_offsets in
  let bind_c_stmts =
    List.filter_map (gen_c_bind state) conditions_with_offsets
  in
  let mapbind_c_stmts = List.filter_map gen_c_mapbind conditions_with_offsets in
  let bound_identifiers = get_all_bound_identifiers conditions in
  let guards_c_exprs =
    gen_c_guards guards bound_identifiers state.currently_defined_bv_sizes
  in
  let consequences_c_stmts = gen_c_consequences state consequences in
  let rule_c_code =
    nest mapbind_c_stmts guards_c_exprs bind_c_stmts consequences_c_stmts
  in
  let Assign_node_type rule_name, _ = consequences in
  let rule_comment_start =
    "\n//----------------------------" ^ rule_name
    ^ "------------------------------//\n"
  in
  let rule_comment_end =
    "\n\
     //------------------------------------------------------------------------------------//\n"
  in
  match assert_c_exprs with
  | [] -> rule_comment_start ^ "{" ^ rule_c_code ^ "}" ^ rule_comment_end
  | _ ->
      rule_comment_start ^ "if ("
      ^ String.concat "&&" assert_c_exprs
      ^ ") {" ^ rule_c_code ^ "}" ^ rule_comment_end

let gen_c_decoder state decoder =
  String.concat "" (List.map (gen_c_rule state) decoder)

let decoder_to_c ?(c_proc_name = "decode") decoder walker =
  let defines =
    "#define SLICE_BITVEC(v, s, e)"
    ^ "((v >> s) & ((((uint64_t)1) << (e - s + 1)) - 1)) \n\n\n"
    ^ "#define INDEX_BITVEC(v, i) ((v >> i) & 1) \n\n\n"
  in
  let procedure_start =
    "static void " ^ c_proc_name ^ "(struct " ^ ast_sail_def_name ^ " *"
    ^ ast_c_parameter ^ ", uint64_t " ^ binary_stream_c_parameter
    ^ ", RVContext *ctx)"
  in
  let initial_state =
    { typedef_walker = walker; currently_defined_bv_sizes = Hashtbl.create 100 }
  in
  let procedure_body = gen_c_decoder initial_state decoder in
  defines ^ procedure_start ^ "{" ^ procedure_body ^ "}"
