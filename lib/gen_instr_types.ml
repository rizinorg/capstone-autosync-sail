open Sail_analysis
open Hashset

type config = { exclude_enums : string set }

type instruction_types = Types of int * string list | Same_as of string
type instr_types = (string, instruction_types) Hashtbl.t

let read_config path =
  let c = { exclude_enums = Hashtbl.create 10 } in
  List.iter (fun e -> set_add c.exclude_enums e) (Utils.read_file path);
  c

let add_instr_types conf case_names_to_instr_types seen_enums analysis case_name
    enum_typenames =
  let do_add name types = Hashtbl.add case_names_to_instr_types name types in
  let applicable_enums =
    List.filter
      (fun (_, name) -> not (set_contains conf.exclude_enums name))
      enum_typenames
  in
  match applicable_enums with
  | [] -> do_add case_name (Types (-1, [case_name]))
  | [(i, enum_typename)] ->
      if not (Hashtbl.mem seen_enums enum_typename) then (
        do_add case_name
          (Types (i, get_all_members_of_enum analysis enum_typename));
        Hashtbl.add seen_enums enum_typename case_name
      )
      else (
        let earlier_case = Hashtbl.find seen_enums enum_typename in
        do_add case_name (Same_as earlier_case)
      )
  | _ ->
      failwith
        ("Ambiguous instruction type enum: can't determine the type enum for \
          ast case " ^ case_name
        )

let gen_instr_types analysis conf =
  let case_names_to_enum_typenames = get_all_cases_with_enum_members analysis in
  let instr_types : instr_types =
    Hashtbl.create (Hashtbl.length case_names_to_enum_typenames)
  in
  let seen_enums = Hashtbl.create 10 in
  Hashtbl.iter
    (add_instr_types conf instr_types seen_enums analysis)
    case_names_to_enum_typenames;
  instr_types
