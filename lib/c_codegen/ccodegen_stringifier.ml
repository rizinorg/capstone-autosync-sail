open Capstone_autosync_sail

open Stringifier
open Constants
open Gen_clike_typedef
open Sail_values
open Utils
open Hashset

type assembler_stringification_state = {
  walker : typedef_walker;
  already_defined_tables : string set;
}

let tostr_tbl_to_c { walker; already_defined_tables } name idx arg_idx tbl =
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

let intrinsic_logic_arg_to_c walker arg =
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

let tostr_logic_to_c ({ walker; _ } as str_state) i tostr =
  match tostr with
  | Lit s -> ("SStream_concat(ss, \"" ^ s ^ "\");", "")
  | Bitv2Str (name, arg_idx, tbl) -> tostr_tbl_to_c str_state name i arg_idx tbl
  | Enum2Str (name, arg_idx, tbl) -> tostr_tbl_to_c str_state name i arg_idx tbl
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
      let args = List.map (intrinsic_logic_arg_to_c walker) args in
      (name ^ "(" ^ String.concat ", " args ^ sep ^ "ss, ctx);", "")

let subcase_body_to_c ({ walker; _ } as str_state) body =
  let tostrs_and_tables = List.mapi (tostr_logic_to_c str_state) body in
  let stringified_tostr = String.concat "" (List.map fst tostrs_and_tables) in
  let tables = String.concat "" (List.map snd tostrs_and_tables) in

  (stringified_tostr, tables)

let clause_subcase_to_c ({ walker; _ } as str_state)
    (subcase_condition, subcase_body) =
  let subcase_body, tables = subcase_body_to_c str_state subcase_body in
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

let assembler_clause_to_c ({ walker; _ } as str_state) (case_name, subcases) =
  set_walker_case walker case_name;
  let subconds_and_tables = List.map (clause_subcase_to_c str_state) subcases in
  let subconds = List.map fst subconds_and_tables in
  let tables = List.map snd subconds_and_tables in
  let clause =
    "case "
    ^ add_prefix_unless_exists identifier_prefix case_name
    ^ ": " ^ String.concat "" subconds ^ "break;"
  in
  (clause, String.concat "" tables)

let assembler_to_c asm walker =
  let procedure_start =
    "static void ast2str(struct " ^ ast_sail_def_name ^ " *" ^ ast_c_parameter
    ^ ", SStream *ss, RVContext *ctx) { " ^ "switch (" ^ ast_c_parameter ^ "->"
    ^ ast_sail_def_name ^ generated_ast_enum_suffix ^ ") {"
  in
  let procedure_end = "}}" in
  let initial_state = { walker; already_defined_tables = Hashtbl.create 100 } in
  let body_and_tables = List.map (assembler_clause_to_c initial_state) asm in
  let procedure_body = String.concat "" (List.map fst body_and_tables) in
  let tables = String.concat "" (List.map snd body_and_tables) in
  (procedure_start ^ procedure_body ^ procedure_end, tables)
