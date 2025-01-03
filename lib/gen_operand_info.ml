open Libsail
open Ast

open Sail_ast_foreach
open Sail_ast_processor
open Sail_utils
open Sail_analysis

(* The four register files in RISCV and its standard extensions *)
(* The float and double register files overlap, but they should still
   be counted as seperate for max clarity of information provided
   by the generated disassembler *)
type regfile = Base | Float | Double | Vector
type operand =
  | Reg of int * regfile
  | Memory of int * int * regfile
  | Imm of int

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

let infer_regfile_getter_or_setter name args operand_names =
  let (E_aux (arg, _)) = List.nth args 0 in
  match arg with
  | E_id i when List.mem (id_to_str i) operand_names ->
      let arg_name = id_to_str i in
      let regfile =
        match id_to_str name with
        | "rX_bits" | "rX" | "wX_bits" | "wX" -> Some (arg_name, Base)
        | "rF_bits" | "rF" | "rF_or_X_S" | "wF_bits" | "wF" | "wF_or_X_S" ->
            Some (arg_name, Float)
        | "rF_or_X_D" | "wF_or_X_D" -> Some (arg_name, Double)
        | "rV_bits" | "rV" | "wV_bits" | "wV" -> Some (arg_name, Vector)
        | _ -> None
      in
      regfile
  | _ -> None

let rec infer_operand_types ?(flag = false) regidx_functions_registery
    operand_names fun_body =
  let operand_types = Hashtbl.create (List.length operand_names) in
  let infer_regfile _ name args =
    match infer_regfile_getter_or_setter name args operand_names with
    | Some (name, regfile) -> Hashtbl.add operand_types name regfile
    | None ->
        infer_regfile_across_function_call regidx_functions_registery
          operand_names name args operand_types
  in
  let processor = { default_expr_processor with process_app = infer_regfile } in
  foreach_expr fun_body processor ();
  List.iter
    (fun operand ->
      if (not (Hashtbl.mem operand_types operand)) && flag then
        (* Should fail, eventually *)
        print_endline
          ("Couldnt identify the register file of register operand " ^ operand)
    )
    operand_names;
  operand_types

and infer_regfile_across_function_call regidx_functions_registery operand_names
    name args operand_types =
  if Hashtbl.mem regidx_functions_registery (id_to_str_noexn name) then (
    let operands_occuring_in_args =
      List.mapi (fun i a -> (i, a)) args
      |> List.filter_map (fun (i, a) ->
             match a with
             | E_aux (E_id id, _) when List.mem (id_to_str id) operand_names ->
                 Some (i, id_to_str id)
             | _ -> None
         )
    in
    if List.length operands_occuring_in_args > 0 then (
      let param_names, body =
        Option.get
          (Hashtbl.find regidx_functions_registery (id_to_str_noexn name))
      in
      print_endline ("\n INLINING FUNC @@_" ^ id_to_str_noexn name ^ "_\n ");
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
      Hashtbl.iter
        (fun p a -> print_endline (a ^ " ->BECOMES-> " ^ p))
        params_to_args;
      let result =
        infer_operand_types regidx_functions_registery renamed_operands body
          ~flag:false
      in
      Hashtbl.iter
        (fun ridx_name regfile ->
          let original_name = Hashtbl.find params_to_args ridx_name in
          Hashtbl.add operand_types original_name regfile
        )
        result;
      print_endline ("DONE PROCESSING " ^ id_to_str_noexn name ^ " +++ \n")
    )
  )

let analyze_operands state _ fun_id func =
  let fun_name = id_to_str_noexn fun_id in
  let (Pat_aux (pat, _)) = func in
  let args, body =
    match pat with Pat_exp (a, b) -> (a, b) | Pat_when (a, b, _) -> (a, b)
  in
  if fun_name <> "execute" then (
    if Hashtbl.mem state.regidx_functions_registery fun_name then (
      print_endline
        ("(\n\n ADDING BODY OF FUNC  )" ^ fun_name ^ " \n************");
      Hashtbl.add state.regidx_functions_registery fun_name
        (Some (destructure_id_arglist args, body))
    )
  )
  else (
    let case_name, arg_names = destructure_union_arglist args in
    let reg_indices_members =
      get_all_case_members_of_type_named state.analysis case_name "regidx"
    in
    print_endline
      ("\n############################### BEGIN " ^ case_name
     ^ "#######################################\n"
      );
    let bound_args = bind_regidx_args reg_indices_members arg_names in
    let regidx_arg_names =
      List.filter (fun a -> Hashtbl.mem bound_args a) arg_names
    in
    let operands =
      infer_operand_types state.regidx_functions_registery regidx_arg_names body
        ~flag:true
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
          ^ " )))))))) \n\
            \ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END \
             %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"
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
      let has_regidx_arg =
        List.exists
          (fun (Typ_aux (t, _)) ->
            match t with
            | Typ_app (i, [a]) ->
                id_to_str i = "bitvector" && sail_bitv_size_to_int_noexn a = 5
            | _ -> false
          )
          types
      in
      if has_regidx_arg then
        Hashtbl.add state.regidx_functions_registery (id_to_str_noexn id) None
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
