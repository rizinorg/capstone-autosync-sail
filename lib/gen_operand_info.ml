open Libsail
open Ast

open Sail_ast_foreach
open Sail_ast_processor
open Sail_utils
open Sail_analysis

open Utils

(* The four register files in RISCV and its standard extensions *)
(* The float and double register files overlap, but they should still
   be counted as seperate for max clarity of information provided
   by the generated disassembler *)
type regfile = Base | Float | Double | Vector
type operand = Reg of int * regfile

type operand_info = (string, operand list) Hashtbl.t

type 'a operand_gen_iteration_state = {
  analysis : sail_analysis_result;
  (* stores bodies of functions that take at least one regidx argument,
     along with the names of formal parameters *)
  regidx_functions_registery :
    (string, (string list * 'a exp) option) Hashtbl.t;
  (* operand details, generated incrementally during iteration of AST *)
  info : operand_info;
}

let bind_regidx_args reg_indices_members arg_names =
  let binding = Hashtbl.create (List.length reg_indices_members) in
  List.iteri
    (fun i arg_name ->
      if List.mem i reg_indices_members then Hashtbl.add binding arg_name i
    )
    arg_names;
  binding

let rec infer_operand_types regidx_functions_registery operand_names fun_body =
  let operand_types = Hashtbl.create (List.length operand_names) in
  let rec do_infer exp =
    let (E_aux (e, _)) = exp in
    match e with
    | E_block children -> List.iter do_infer children
    | E_typ (_, child) -> do_infer child
    | E_app (name, args) when List.length args = 1 -> (
        try
          let (E_aux (arg, _)) = Option.get (get_sole_element_or_none args) in
          match arg with
          | E_id i when List.mem (id_to_str i) operand_names ->
              let regfile =
                match id_to_str name with
                | "rX_bits" | "rX" -> Base
                | "rF_bits" | "rF" | "rF_or_X_S" -> Float
                | "rF_or_X_D" -> Double
                | "rV_bits" | "rV" -> Vector
                | _ -> invalid_arg "Not a valid register file"
              in
              Hashtbl.add operand_types (id_to_str i) regfile
          | _ -> invalid_arg "Not an expected overload call"
        with Invalid_argument _ -> List.iter do_infer args
      )
    | E_app (name, args) -> (
        try
          let (E_aux (arg, _)) = List.nth args 0 in
          match arg with
          | E_id i when List.mem (id_to_str i) operand_names ->
              let regfile =
                match id_to_str name with
                | "wX_bits" | "wX" -> Base
                | "wF_bits" | "wF" | "wF_or_X_S" -> Float
                | "wF_or_X_D" -> Double
                | "wV_bits" | "wV" -> Vector
                | _ -> invalid_arg "Not a valid register file"
              in
              Hashtbl.add operand_types (id_to_str i) regfile
          | _ -> invalid_arg "Not an expected overload call"
        with Invalid_argument _ ->
          List.iter do_infer args;
          if Hashtbl.mem regidx_functions_registery (id_to_str_noexn name) then (
            let operands_occuring_in_args =
              List.mapi (fun i a -> (i, a)) args
              |> List.filter_map (fun (i, a) ->
                     match a with
                     | E_aux (E_id id, _)
                       when List.mem (id_to_str id) operand_names ->
                         Some (i, id_to_str id)
                     | _ -> None
                 )
            in
            if List.length operands_occuring_in_args > 0 then (
              let param_names, body =
                Option.get
                  (Hashtbl.find regidx_functions_registery (id_to_str_noexn name)
                  )
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
                infer_operand_types regidx_functions_registery renamed_operands
                  body
              in
              Hashtbl.iter
                (fun ridx_name regfile ->
                  let original_name = Hashtbl.find params_to_args ridx_name in
                  Hashtbl.add operand_types original_name regfile
                )
                result
            )
          )
      )
    | E_app_infix (child1, op, child2) ->
        do_infer child1;
        do_infer child2
    | E_tuple children -> List.iter do_infer children
    | E_if (child1, child2, child3) ->
        do_infer child1;
        do_infer child2;
        do_infer child3
    | E_loop (_, _, child1, child2) ->
        do_infer child1;
        do_infer child2
    | E_for (_, child1, child2, child3, _, child4) ->
        do_infer child1;
        do_infer child2;
        do_infer child3;
        do_infer child4
    | E_vector children -> List.iter do_infer children
    | E_vector_access (child1, child2) ->
        do_infer child1;
        do_infer child2
    | E_vector_subrange (child1, child2, child3) ->
        do_infer child1;
        do_infer child2;
        do_infer child3
    | E_vector_update (child1, child2, child3) ->
        do_infer child1;
        do_infer child2;
        do_infer child3
    | E_vector_update_subrange (child1, child2, child3, child4) ->
        do_infer child1;
        do_infer child2;
        do_infer child3;
        do_infer child4
    | E_vector_append (child1, child2) ->
        do_infer child1;
        do_infer child2
    | E_list children -> List.iter do_infer children
    | E_cons (child1, child2) ->
        do_infer child1;
        do_infer child2
    | E_struct children ->
        List.iter
          (fun (FE_aux (FE_fexp (_, child), _)) -> do_infer child)
          children
    | E_struct_update (child, children) ->
        List.iter
          (fun (FE_aux (FE_fexp (_, child), _)) -> do_infer child)
          children;
        do_infer child
    | E_field (child, _) -> do_infer child
    | E_match (child, children) ->
        List.iter
          (fun (Pat_aux (c, _)) ->
            match c with
            | Pat_exp (_, child) -> do_infer child
            | Pat_when (_, child1, child2) ->
                do_infer child1;
                do_infer child2
          )
          children;
        do_infer child
    | E_let (LB_aux (LB_val (_, child1), _), child2) ->
        do_infer child1;
        do_infer child2
    | E_assign (_, child) -> do_infer child
    | E_return child -> do_infer child
    | E_exit child -> do_infer child
    | E_throw child -> do_infer child
    | E_try (child, children) ->
        List.iter
          (fun (Pat_aux (c, _)) ->
            match c with
            | Pat_exp (_, child) -> do_infer child
            | Pat_when (_, child1, child2) ->
                do_infer child1;
                do_infer child2
          )
          children;
        do_infer child
    | E_assert (child1, child2) ->
        do_infer child1;
        do_infer child2
    | _ -> ()
  in
  do_infer fun_body;
  operand_types

let analyze_operands state _ fun_id func =
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
    let bound_args = bind_regidx_args reg_indices_members arg_names in
    let operands =
      infer_operand_types state.regidx_functions_registery arg_names body
    in
    print_endline
      ("+++++++++++++++++++++++++++ IN CASE " ^ case_name
     ^ " +++++++++++++++++++++++++++"
      );
    Hashtbl.iter
      (fun name regfile ->
        print_endline
          ("(((((((( ARG " ^ name ^ " IS "
          ^ ( match regfile with
            | Base -> "INTEGER REGISTER "
            | Float -> "FLOATING POINT REGISTER "
            | Double -> "DOUBLE FLOATING POINT REGISTER "
            | Vector -> "VECTOR REGISTER "
            )
          ^ " )))))))) \n\n"
          )
      )
      operands
  )
let add_regidx_function state _ id _ typ =
  print_endline
    ("\n $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ CHECKING SIG OF "
   ^ id_to_str_noexn id ^ " $$$$$$$$$$$$$$$$$$$$$$ \n"
    );
  let (Typ_aux (t, _)) = typ in
  match t with
  | Typ_fn (types, _) ->
      List.iter
        (fun t ->
          let (Typ_aux (t, _)) = t in
          match t with
          | Typ_id i when id_to_str i = "regidx" ->
              Hashtbl.add state.regidx_functions_registery (id_to_str id) None
          | _ -> ()
        )
        types
  | _ -> ()

let gen_operand_info ast analysis =
  let processor =
    {
      default_processor with
      process_function_clause = analyze_operands;
      process_val = add_regidx_function;
    }
  in
  let state =
    {
      analysis;
      regidx_functions_registery = Hashtbl.create 500;
      info = Hashtbl.create 300;
    }
  in
  foreach_node ast processor state;
  print_endline
    "\n\n\
     ================================== OPERAND ANALYSIS DONE \
     =============================================== \n\n\
    \ "
