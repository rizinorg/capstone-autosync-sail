open Libsail
open Ast
open Ast_defs
open Sail_ast_processor

let foreach_val node proc state =
  let (VS_aux (child, _)) = node in
  let (VS_val_spec (type_scheme, id, _)) = child in
  let (TypSchm_aux (tschm, _)) = type_scheme in
  let (TypSchm_ts (quantifier, typ)) = tschm in
  proc.process_val state child id quantifier typ

let foreach_fundef node proc state =
  let (FD_aux (child, _)) = node in
  let (FD_function (_, _, clauses)) = child in
  List.iter
    (fun c ->
      let (FCL_aux (clause, _)) = c in
      let (FCL_funcl (id, pexp)) = clause in
      proc.process_function_clause state clause id pexp
    )
    clauses

let foreach_mapdef node proc state =
  let (MD_aux (mapping, _)) = node in
  let (MD_mapping (id, tannot, clauses)) = mapping in
  proc.process_mapping state mapping id tannot clauses;
  List.iter
    (fun cl ->
      let (MCL_aux (clause, _)) = cl in
      match clause with
      | MCL_bidir (pat1, pat2) ->
          proc.process_mapping_bidir_clause state clause id tannot pat1 pat2
      | _ -> ()
    )
    clauses

let foreach_let node _ _ = match node with LB_aux (_, _) -> ()

let foreach_typedef_aux node proc state =
  match node with
  | TD_abbrev (id, typquant, arg) ->
      proc.process_typedef state node id;
      proc.process_abbrev state node id typquant arg
  | TD_record (id, typquant, members, flg) ->
      proc.process_typedef state node id;
      proc.process_record state node id typquant members flg
  | TD_variant (id, typquant, clauses, flg) ->
      proc.process_typedef state node id;
      proc.process_union state node id typquant clauses flg;
      List.iter
        (fun tu ->
          let (Tu_aux (type_union, _)) = tu in
          let (Tu_ty_id (typ, clause_id)) = type_union in
          proc.process_union_clause state type_union id clause_id typ
        )
        clauses
  | TD_enum (id, cases, flg) ->
      proc.process_typedef state node id;
      proc.process_enum state node id cases flg
  | TD_bitfield (id, typ, members) ->
      proc.process_typedef state node id;
      proc.process_bitfield state node id typ members
  | TD_abstract _ -> ()

let foreach_typedef node proc state =
  match node with TD_aux (child, _) -> foreach_typedef_aux child proc state

let foreach_defaux node proc state =
  match node with
  | DEF_type child -> foreach_typedef child proc state
  | DEF_let child -> foreach_let child proc state
  | DEF_mapdef child -> foreach_mapdef child proc state
  | DEF_fundef child -> foreach_fundef child proc state
  | DEF_val child -> foreach_val child proc state
  | _ -> ()

let foreach_def node proc state =
  match node with DEF_aux (child, _) -> foreach_defaux child proc state

let foreach_defs node proc state =
  List.iter (fun def -> foreach_def def proc state) node.defs

let foreach_node root processor init_state =
  foreach_defs root processor init_state

let rec foreach_expr e processor state =
  let (E_aux (ex, _)) = e in
  match ex with
  | E_lit l -> processor.process_lit state l
  | E_id i -> processor.process_id state i
  | E_ref r -> processor.process_ref state r
  | E_var (lhs, child1, child2) ->
      processor.process_var state lhs child1 child2;
      foreach_expr child1 processor state;
      foreach_expr child2 processor state
  | E_block children ->
      processor.process_block state children;
      List.iter (fun c -> foreach_expr c processor state) children
  | E_typ (ty, child) ->
      processor.process_typ state ty child;
      foreach_expr child processor state
  | E_app (name, args) ->
      processor.process_app state name args;
      List.iter (fun c -> foreach_expr c processor state) args
  | E_app_infix (child1, op, child2) ->
      processor.process_app_infix state child1 op child2;
      foreach_expr child1 processor state;
      foreach_expr child2 processor state
  | E_tuple children ->
      processor.process_tuple state children;
      List.iter (fun c -> foreach_expr c processor state) children
  | E_if (child1, child2, child3) ->
      print_endline "\n((((((((((((((((((FOUND IF ELSE)))))))))))))))))))))))))";
      processor.process_if state child1 child2 child3;
      foreach_expr child1 processor state;
      foreach_expr child2 processor state;
      foreach_expr child3 processor state
  | E_loop (l, m, child1, child2) ->
      processor.process_loop state l m child1 child2;
      foreach_expr child1 processor state;
      foreach_expr child2 processor state
  | E_for (i, child1, child2, child3, o, child4) ->
      processor.process_for state i child1 child2 child3 o child4;
      foreach_expr child1 processor state;
      foreach_expr child2 processor state;
      foreach_expr child3 processor state;
      foreach_expr child4 processor state
  | E_vector children ->
      processor.process_vector state children;
      List.iter (fun c -> foreach_expr c processor state) children
  | E_vector_access (child1, child2) ->
      processor.process_vector_access state child1 child2;
      foreach_expr child1 processor state;
      foreach_expr child2 processor state
  | E_vector_subrange (child1, child2, child3) ->
      processor.process_vector_subrange state child1 child2 child3;
      foreach_expr child1 processor state;
      foreach_expr child2 processor state;
      foreach_expr child3 processor state
  | E_vector_update (child1, child2, child3) ->
      processor.process_vector_update state child1 child2 child3;
      foreach_expr child1 processor state;
      foreach_expr child2 processor state;
      foreach_expr child3 processor state
  | E_vector_update_subrange (child1, child2, child3, child4) ->
      processor.process_vector_update_subrange state child1 child2 child3 child4;
      foreach_expr child1 processor state;
      foreach_expr child2 processor state;
      foreach_expr child3 processor state;
      foreach_expr child4 processor state
  | E_vector_append (child1, child2) ->
      processor.process_vector_append state child1 child2;
      foreach_expr child1 processor state;
      foreach_expr child2 processor state
  | E_list children ->
      processor.process_list state children;
      List.iter (fun c -> foreach_expr c processor state) children
  | E_cons (child1, child2) ->
      processor.process_cons state child1 child2;
      foreach_expr child1 processor state;
      foreach_expr child2 processor state
  | E_struct children ->
      processor.process_struct state children;
      List.iter
        (fun (FE_aux (FE_fexp (_, c), _)) -> foreach_expr c processor state)
        children
  | E_struct_update (child, children) ->
      processor.process_struct_update state child children;
      List.iter
        (fun (FE_aux (FE_fexp (_, c), _)) -> foreach_expr c processor state)
        children;
      foreach_expr child processor state
  | E_field (child, i) ->
      processor.process_field state child i;
      foreach_expr child processor state
  | E_match (child, children) ->
      processor.process_match state child children;
      List.iter
        (fun (Pat_aux (c, _)) ->
          match c with
          | Pat_exp (_, child) -> foreach_expr child processor state
          | Pat_when (_, child1, child2) ->
              foreach_expr child1 processor state;
              foreach_expr child2 processor state
        )
        children;
      foreach_expr child processor state
  | E_let ((LB_aux (LB_val (_, rhs), _) as lb), body) ->
      processor.process_let state lb body;
      foreach_expr rhs processor state;
      foreach_expr body processor state
  | E_assign (lhs, child) ->
      processor.process_assign state lhs child;
      foreach_expr child processor state
  | E_return child ->
      processor.process_return state child;
      foreach_expr child processor state
  | E_exit child ->
      processor.process_exit state child;
      foreach_expr child processor state
  | E_throw child ->
      processor.process_throw state child;
      foreach_expr child processor state
  | E_try (child, children) ->
      processor.process_try state child children;
      List.iter
        (fun (Pat_aux (c, _)) ->
          match c with
          | Pat_exp (_, child) -> foreach_expr child processor state
          | Pat_when (_, child1, child2) ->
              foreach_expr child1 processor state;
              foreach_expr child2 processor state
        )
        children;
      foreach_expr child processor state
  | E_assert (child1, child2) ->
      processor.process_assert state child1 child2;
      foreach_expr child1 processor state;
      foreach_expr child2 processor state
  | _ -> ()
