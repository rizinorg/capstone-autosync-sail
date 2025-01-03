open Capstone_autosync_sail

open Sail_values
open Decode_procedure
open Constants
open Gen_clike_typedef
open Utils

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
               | length_prev :: _ ->
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
