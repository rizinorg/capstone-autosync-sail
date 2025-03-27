open Libsail
open Ast
open Lexing
open Ast_util

open Utils

let stringify_lex_pos start ending =
  if start.pos_fname = ending.pos_fname then
    "file " ^ start.pos_fname ^ " @ [line: "
    ^ string_of_int start.pos_lnum
    ^ ", column: "
    ^ string_of_int start.pos_cnum
    ^ "]..." ^ "[line: "
    ^ string_of_int ending.pos_lnum
    ^ ", column: "
    ^ string_of_int ending.pos_cnum
    ^ "]"
  else "<multi-file element>"

let stringify_sail_source_loc loc =
  let open Parse_ast in
  match loc with
  | Range (start, ending) -> stringify_lex_pos start ending
  | Unknown -> "<unknown location>"
  | _ -> "<unprintable>"

let id_to_str id =
  let (Id_aux (i, loc)) = id in
  match i with
  | Id s -> s
  | Operator o ->
      failwith
        ("Operator identifiers are not supported @ "
        ^ stringify_sail_source_loc loc
        ^ "(Operator: " ^ o ^ ")"
        )
let id_to_str_noexn id =
  let (Id_aux (i, loc)) = id in
  match i with Id s -> s | Operator o -> o

let convert_bitv_size_to_int ?(throw_on_unsupported_size_exprs = true) size =
  let (A_aux (a, _)) = size in
  match a with
  | A_nexp (Nexp_aux (nexp, loc)) -> (
      match nexp with
      | Nexp_constant sz -> Nat_big_num.to_int sz
      | _ ->
          if throw_on_unsupported_size_exprs then
            failwith
              ("Unsupported size expression in call to built-in type \
                constructor 'bits', only constants are supported @ "
              ^ stringify_sail_source_loc loc
              );
          -1
    )
  | _ -> failwith "UNREACHABLE"

let sail_bitv_size_to_int =
  convert_bitv_size_to_int ~throw_on_unsupported_size_exprs:true

let sail_bitv_size_to_int_noexn =
  convert_bitv_size_to_int ~throw_on_unsupported_size_exprs:false

let bitv_literal_to_str bitv_lit =
  let (L_aux (lit, _)) = bitv_lit in
  match lit with
  | L_hex lit_str -> "0x" ^ lit_str
  | L_bin lit_str -> "0x" ^ binary_str_to_hex_str lit_str
  | _ -> failwith "Expected a bitvec literal, found neither L_hex nor L_bin"

let bitv_literal_size bitv_lit =
  let (L_aux (lit, _)) = bitv_lit in
  match lit with
  | L_hex lit_str -> String.length lit_str * 4
  | L_bin lit_str -> String.length lit_str
  | _ -> failwith "Expected a bitvec literal, found neither L_hex nor L_bin"

let bindings_contains_id bindings i =
  Option.is_some (Bindings.find_opt i bindings)

let destructure_type_annotation type_annotation =
  match type_annotation with
  | Typ_annot_opt_some (_, Typ_aux (Typ_bidir (t1, t2), _)) ->
      let (Typ_aux (type1, _)) = t1 in
      let (Typ_aux (type2, _)) = t2 in
      Some (type1, type2)
  | _ -> None

let is_mapping_from_string_to_type_id mapping_type_annotation
    other_type_predicate =
  let types = destructure_type_annotation mapping_type_annotation in
  match types with
  | Some (Typ_id id1, Typ_id id2)
    when id_to_str id1 = "string" && other_type_predicate id2 ->
      Some (id_to_str id2)
  | Some (Typ_id id1, Typ_id id2)
    when id_to_str id2 = "string" && other_type_predicate id1 ->
      Some (id_to_str id1)
  | _ -> None

let is_mapping_from_bitv_to_type_id mapping_type_annotation other_type_predicate
    =
  let types = destructure_type_annotation mapping_type_annotation in
  match types with
  | Some (Typ_app (id1, args), Typ_id id2)
    when (id_to_str id1 = "bits" || id_to_str id1 = "bitvector")
         && other_type_predicate id2 ->
      Some (id_to_str id2, sail_bitv_size_to_int (List.nth args 0))
  | Some (Typ_id id1, Typ_app (id2, args))
    when (id_to_str id2 = "bits" || id_to_str id2 = "bitvector")
         && other_type_predicate id1 ->
      Some (id_to_str id1, sail_bitv_size_to_int (List.nth args 0))
  | _ -> None

let destructure_mapping mapping_clauses typ_name
    (inner_destructurer :
      'a mpat_aux -> 'a mpat_aux -> l -> string * 'a mpat_aux
      ) =
  let destructure_clause cl =
    let (MCL_aux (clause, _)) = cl in
    match clause with
    | MCL_bidir (l, r) -> (
        let (MPat_aux (left, _)) = l in
        let (MPat_aux (right, _)) = r in
        match (left, right) with
        | MPat_pat lpat, MPat_pat rpat ->
            let (MP_aux (p1, (loc, _))) = lpat in
            let (MP_aux (p2, _)) = rpat in
            inner_destructurer p1 p2 loc
        | _ ->
            failwith
              ("Both sides of a bidirectional T <-> " ^ typ_name
             ^ " mapping must be simple patterns"
              )
      )
    | _ ->
        failwith
          ("Non-bidirectional T <-> " ^ typ_name ^ " mappings are not supported")
  in

  List.map destructure_clause mapping_clauses

let destructure_bitv_mapping mapping_clauses =
  destructure_mapping mapping_clauses "bitvec" (fun p1 p2 loc ->
      match (p1, p2) with
      | pat, MP_lit bv -> (bitv_literal_to_str bv, pat)
      | MP_lit bv, pat -> (bitv_literal_to_str bv, pat)
      | _ ->
          failwith
            ("bitvec mappings with bitvec expressions more complex than \
              literals are not supported @"
            ^ stringify_sail_source_loc loc
            )
  )

let destructure_string_mapping mapping_clauses =
  destructure_mapping mapping_clauses "string" (fun p1 p2 loc ->
      match (p1, p2) with
      | pat, MP_lit (L_aux (L_string s, _)) -> (s, pat)
      | MP_lit (L_aux (L_string s, _)), pat -> (s, pat)
      | _ ->
          failwith
            ("string mappings with expressions more complex than literals are \
              not supported @ "
            ^ stringify_sail_source_loc loc
            )
  )

let destructure_union_arglist args =
  let (P_aux (args_pat, (l, _))) = args in
  match args_pat with
  | P_app (id, arg_names) -> (
      let args_tuple = Option.get (get_sole_element_or_none arg_names) in
      match args_tuple with
      | P_aux (P_tuple args, (l, _)) ->
          ( id_to_str id,
            List.map
              (fun (P_aux (p, _)) ->
                match p with
                | P_id i -> id_to_str i
                | _ ->
                    failwith
                      ("Trying to destructure a union arglist with non-id \
                        parameter @ "
                      ^ stringify_sail_source_loc l
                      )
              )
              args
          )
      | P_aux (P_id i, (l, _)) -> (id_to_str id, [id_to_str i])
      | _ -> (id_to_str id, [])
    )
  | _ ->
      failwith
        ("Trying to destructure a non-union arglist @ "
        ^ stringify_sail_source_loc l
        )

let destructure_id_arglist args =
  let (P_aux (args, (l, _))) = args in
  match args with
  | P_tuple args ->
      List.map
        (fun (P_aux (p, (l, _))) ->
          match p with
          | P_id i -> id_to_str i
          | P_typ (_, P_aux (P_id i, _)) -> id_to_str i
          | _ ->
              failwith
                ("Trying to destructure an id arglist but found a non-id \
                  parameter @ "
                ^ stringify_sail_source_loc l
                )
        )
        args
  | P_typ (_, P_aux (P_id i, _)) -> [id_to_str i]
  | P_id i -> [id_to_str i]
  | _ ->
      failwith
        ("Trying to destructure an non-id arglist @ "
        ^ stringify_sail_source_loc l
        )
