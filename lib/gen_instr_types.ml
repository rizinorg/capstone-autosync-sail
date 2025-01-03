open Sail_analysis
open Hashset

type config = { exclude_enums : string set }

type instr_types = (string, int * string list) Hashtbl.t

let read_config path =
  let c = { exclude_enums = Hashtbl.create 10 } in
  List.iter (fun e -> set_add c.exclude_enums e) (Utils.read_file path);
  c

let add_instr_types conf case_names_to_instr_types analysis case_name
    enum_typenames =
  let do_add name types = Hashtbl.add case_names_to_instr_types name types in
  let applicable_enums =
    List.filter
      (fun (_, name) -> not (set_contains conf.exclude_enums name))
      enum_typenames
  in
  match applicable_enums with
  | [] -> do_add case_name (-1, [case_name])
  | [(i, enum_typename)] ->
      do_add case_name (i, get_all_members_of_enum analysis enum_typename)
  | _ -> failwith "FAIL"

let gen_instr_types analysis conf =
  let case_names_to_enum_typenames = get_all_cases_with_enum_members analysis in
  let instr_types =
    Hashtbl.create (Hashtbl.length case_names_to_enum_typenames)
  in
  Hashtbl.iter
    (add_instr_types conf instr_types analysis)
    case_names_to_enum_typenames;
  instr_types
