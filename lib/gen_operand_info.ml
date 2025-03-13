open Libsail
open Ast

open Sail_ast_foreach
open Sail_ast_processor
open Sail_utils
open Sail_analysis

open Utils

open Hashset

(* The four register files in RISCV and its standard extensions *)
(* The float and double register files overlap, but they should still
   be counted as seperate register files for max clarity of information *)
type regfile = Base | Float | Double | Vector
type regaccess = Read | Write
type reg_operand = Reg of int * regfile * regaccess

type imm_operand = Imm of int

type operand_info = {
  registers_info : (string, reg_operand list) Hashtbl.t;
  immediates_info : (string, imm_operand list) Hashtbl.t;
}

type 'a operand_gen_iteration_state = {
  analysis : sail_analysis_result;
  (* stores bodies of functions that take at least one regidx argument,
     along with the names of formal parameters *)
  regidx_functions_registery :
    (string, (string list * 'a exp) option) Hashtbl.t;
  (* operand details, generated incrementally during iteration of AST *)
  op_info : operand_info;
}

let bind_regidx_args reg_indices_members arg_names =
  let binding = Hashtbl.create (List.length reg_indices_members) in
  List.iteri
    (fun i arg_name ->
      if List.mem i reg_indices_members then Hashtbl.add binding arg_name i
    )
    arg_names;
  binding

let infer_regfile_getter_or_setter name args operand_names =
  let (E_aux (arg, _)) = List.nth args 0 in
  match arg with
  | E_id i when set_contains operand_names (id_to_str i) ->
      let arg_name = id_to_str i in
      let regfile =
        match id_to_str_noexn name with
        | "rX_bits" | "rX" -> Some (arg_name, Base, Read)
        | "wX_bits" | "wX" -> Some (arg_name, Base, Write)
        | "rF_bits" | "rF" | "rF_or_X_S" -> Some (arg_name, Float, Read)
        | "wF_bits" | "wF" | "wF_or_X_S" -> Some (arg_name, Float, Write)
        | "rF_or_X_D" -> Some (arg_name, Double, Read)
        | "wF_or_X_D" -> Some (arg_name, Double, Write)
        | "rV_bits" | "rV" -> Some (arg_name, Vector, Read)
        | "wV_bits" | "wV" -> Some (arg_name, Vector, Write)
        | _ -> None
      in
      regfile
  | _ -> None

let get_names_in_expr exp =
  let names = ref [] in
  let processor =
    {
      default_expr_processor with
      process_id = (fun _ i -> names := id_to_str i :: !names);
    }
  in
  foreach_expr exp processor ();
  !names

let get_names_in_letbind (LB_aux (LB_val (lhs, rhs), _)) =
  match lhs with
  | P_aux (P_id l, _) | P_aux (P_typ (_, P_aux (P_id l, _)), _) ->
      let l_name = id_to_str l in
      let r_names = get_names_in_expr rhs in
      if List.length r_names <> 0 then Some (l_name, r_names) else None
  | _ -> None

let get_names_occuring_in_exprs exprs =
  let indexed_exprs = List.mapi (fun i a -> (i, a)) exprs in
  List.filter_map
    (fun (i, exp) ->
      let name = ref "" in
      let processor =
        {
          default_expr_processor with
          process_id =
            (fun _ i ->
              if !name <> "" then failwith "Expr must be contain at most on id"
              else name := id_to_str i
            );
        }
      in
      foreach_expr exp processor ();
      if !name <> "" then Some (i, !name) else None
    )
    indexed_exprs

let rec infer_register_files ?(rootcall = false) regidx_fun_registery
    inferred_regfiles op_names fun_body =
  let operand_types = Hashtbl.create (set_length op_names) in
  let aliases_to_operands = Hashtbl.create 10 in
  let infer_regfile _ name args =
    match infer_regfile_getter_or_setter name args op_names with
    | Some (name, regfile, regaccess) ->
        Hashtbl.add operand_types name (regfile, regaccess)
    | None ->
        let result =
          infer_regfile_across_funcall regidx_fun_registery inferred_regfiles
            op_names name args
        in
        Hashtbl.iter (fun k v -> Hashtbl.add operand_types k v) result
  in
  let add_alias _ let_binding _ =
    match get_names_in_letbind let_binding with
    | Some (alias, names)
      when List.exists (fun n -> set_contains op_names n) names ->
        let target = List.find (fun n -> set_contains op_names n) names in
        if not (Hashtbl.mem aliases_to_operands target) then
          Hashtbl.add aliases_to_operands alias target
        else (
          let operand_target = Hashtbl.find aliases_to_operands target in
          Hashtbl.add aliases_to_operands alias operand_target
        );
        set_add op_names alias
    | _ -> ()
  in
  let processor =
    {
      default_expr_processor with
      process_app = infer_regfile;
      process_let = add_alias;
    }
  in
  foreach_expr fun_body processor ();
  Hashtbl.iter
    (fun op _ ->
      if Hashtbl.mem aliases_to_operands op then (
        let original = Hashtbl.find aliases_to_operands op in
        if not (Hashtbl.mem operand_types original) then
          if Hashtbl.mem operand_types op then (
            let regtype = Hashtbl.find operand_types op in
            Hashtbl.add operand_types original regtype
          )
      )
    )
    op_names;
  Hashtbl.iter
    (fun operand _ ->
      if
        (not (Hashtbl.mem operand_types operand))
        && rootcall
        && not (Hashtbl.mem aliases_to_operands operand)
      then
        (* Should fail, eventually *)
        print_endline
          ("Couldnt identify the register file of register operand " ^ operand)
    )
    op_names;
  Hashtbl.filter_map_inplace
    (fun k v -> if Hashtbl.mem aliases_to_operands k then None else Some v)
    operand_types;
  operand_types

and infer_regfile_across_funcall regidx_functions_registery inferred_regfiles
    operand_names name args =
  if Hashtbl.mem regidx_functions_registery (id_to_str_noexn name) then
    infer_regfile_across_function_call regidx_functions_registery
      inferred_regfiles operand_names name args
  else if id_to_str_noexn name = "execute" then
    infer_regfile_across_execute_call inferred_regfiles operand_names args
  else Hashtbl.create 1

and infer_regfile_across_function_call regidx_functions_registery
    inferred_regfiles operand_names name args =
  let operands_occuring_in_args =
    List.mapi (fun i a -> (i, a)) args
    |> List.filter_map (fun (i, a) ->
           let names = get_names_in_expr a in
           if names |> List.exists (fun n -> set_contains operand_names n) then (
             let op_name =
               names |> List.find (fun n -> set_contains operand_names n)
             in
             Some (i, op_name)
           )
           else None
       )
  in
  if List.length operands_occuring_in_args > 0 then (
    let operand_regfiles = Hashtbl.create 20 in
    let param_names, body =
      Option.get (Hashtbl.find regidx_functions_registery (id_to_str_noexn name))
    in
    let params_to_args = Hashtbl.create 10 in
    let renamed_operands =
      List.map
        (fun (i, a) ->
          let param_name = List.nth param_names i in
          Hashtbl.add params_to_args param_name a;
          param_name
        )
        operands_occuring_in_args
    in
    let result =
      infer_register_files regidx_functions_registery inferred_regfiles
        (set_of_list renamed_operands)
        body
    in
    Hashtbl.iter
      (fun ridx_name regfile ->
        let original_name = Hashtbl.find params_to_args ridx_name in
        Hashtbl.add operand_regfiles original_name regfile
      )
      result;
    operand_regfiles
  )
  else Hashtbl.create 1

and infer_regfile_across_execute_call op_info operand_names args =
  match args with
  | [E_aux (E_app (i, args), _)] -> (
      try
        let operand_regfiles = Hashtbl.create 20 in
        let case_name = id_to_str i in
        let case_info = Hashtbl.find op_info case_name in
        let arg_names =
          match args with
          | [E_aux (E_id a, _)] -> [(0, id_to_str a)]
          | [E_aux (E_tuple args, _)] -> get_names_occuring_in_exprs args
          | _ -> failwith "Error: unsupported argument pattern of execute() "
        in
        List.iter
          (fun (idx, name) ->
            if set_contains operand_names name then (
              let (Reg (_, regfile, regaccess)) =
                List.find (fun (Reg (index, _, _)) -> index = idx) case_info
              in
              Hashtbl.add operand_regfiles name (regfile, regaccess)
            )
          )
          arg_names;
        operand_regfiles
      with Not_found ->
        print_endline "Couldn't find an execute clause";
        Hashtbl.create 1
    )
  | _ -> failwith "Error: unsupported pattern of calling execute()"

let infer_registers state _ fun_id func =
  let fun_name = id_to_str_noexn fun_id in
  let (Pat_aux (pat, _)) = func in
  let args, body =
    match pat with Pat_exp (a, b) -> (a, b) | Pat_when (a, b, _) -> (a, b)
  in
  if fun_name <> "execute" then (
    if Hashtbl.mem state.regidx_functions_registery fun_name then
      Hashtbl.add state.regidx_functions_registery fun_name
        (Some (destructure_id_arglist args, body))
  )
  else (
    let case_name, arg_names = destructure_union_arglist args in
    let reg_indices_members =
      get_all_case_members_of_type_named state.analysis case_name "regidx"
    in
    let creg_indices_members =
      get_all_case_members_of_type_named state.analysis case_name "cregidx"
    in
    let reg_indices_members =
      List.append reg_indices_members creg_indices_members
    in
    let bound_args = bind_regidx_args reg_indices_members arg_names in
    let regidx_arg_names =
      List.filter (fun a -> Hashtbl.mem bound_args a) arg_names
    in
    let regidx_names = set_of_list regidx_arg_names in
    let regs =
      infer_register_files state.regidx_functions_registery
        state.op_info.registers_info regidx_names body ~rootcall:true
    in
    Hashtbl.iter
      (fun name (regfile, regaccess) ->
        let case_operands_info =
          if Hashtbl.mem state.op_info.registers_info case_name then
            Hashtbl.find state.op_info.registers_info case_name
          else []
        in
        let index = Hashtbl.find bound_args name in
        let new_info = Reg (index, regfile, regaccess) :: case_operands_info in
        Hashtbl.replace state.op_info.registers_info case_name new_info
      )
      regs
  )
let add_regidx_function state _ id _ typ =
  let (Typ_aux (t, _)) = typ in
  match t with
  | Typ_fn (types, _) ->
      let has_regidx_arg =
        List.exists
          (fun (Typ_aux (t, _)) ->
            match t with
            | Typ_app (i, [a]) ->
                id_to_str i = "bitvector" && sail_bitv_size_to_int_noexn a = 5
            | Typ_id i when id_to_str i = "regidx" -> true
            | _ -> false
          )
          types
      in
      if has_regidx_arg then
        Hashtbl.add state.regidx_functions_registery (id_to_str_noexn id) None
  | _ -> ()

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
                    let imms = List.map (fun index -> Imm index) imm_indices in
                    Hashtbl.add state.op_info.immediates_info
                      (id_to_str case_id) imms
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
      process_val = add_regidx_function;
      process_mapping_bidir_clause = infer_immediates;
    }
  in
  let state =
    {
      analysis;
      regidx_functions_registery = Hashtbl.create 500;
      op_info =
        {
          registers_info = Hashtbl.create 200;
          immediates_info = Hashtbl.create 200;
        };
    }
  in
  foreach_node ast processor state;
  state.op_info
