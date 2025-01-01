open Libsail
open Type_check
open Ast

open Riscv_disasm_from_sail
open Constants
open Gen_decoder
open Gen_assembler
open Stringify

open Printexc

let get_generator_comment () =
  let sail_hash = List.nth (Utils.read_file "conf/hash.txt") 0 in
  let delimiter =
    "======================================================================="
  in
  "/*"
  ^ String.concat "*/\n/*"
      [
        delimiter;
        " This code was generated by the tool auto-sync-sail";
        " (see https://github.com/rizinorg/capstone-autosync-sail)";
        " from the sail model of RISC-V";
        " (see https://github.com/riscv/sail-riscv) @ version " ^ sail_hash
        ^ ".";
        " DO NOT MODIFY THIS CODE MANUALLY. ANY MANUAL EDITS ARE OVERWRITTEN.";
        " ------------------------------------------------------------------- ";
        " Copyright © 2024 moste00 <ubermenchun@gmail.com>";
        " SPDX-License-Identifier: BSD-3-Clause";
        delimiter;
      ]
  ^ "*/\n\n"

let mkdir_if_none_exists dirname =
  try Sys.mkdir dirname 0o777 with Sys_error _ -> ()

let write_c_file ?(additional_includes = []) name code =
  mkdir_if_none_exists "riscv_disasm";

  let oc = open_out ("riscv_disasm/" ^ name) in
  let mk_include_lines incs =
    String.concat "\n"
      (List.map
         (fun i ->
           let delimiter = if i.[0] = '<' || i.[0] = '"' then "" else "\"" in
           "#include " ^ delimiter ^ i ^ delimiter
         )
         incs
      )
    ^ "\n\n"
  in
  let include_string = mk_include_lines includes in
  let additional_includes_string = mk_include_lines additional_includes in
  let name_no_dots = String.map (fun c -> if c = '.' then '_' else c) name in
  Printf.fprintf oc "%s" (get_generator_comment ());
  Printf.fprintf oc "%s"
    ("#ifndef __" ^ String.uppercase_ascii name_no_dots ^ "__\n");
  Printf.fprintf oc "%s"
    ("#define __" ^ String.uppercase_ascii name_no_dots ^ "__\n");
  Printf.fprintf oc "%s" include_string;
  Printf.fprintf oc "%s" additional_includes_string;
  Printf.fprintf oc "%s" code;
  Printf.fprintf oc "%s" "\n #endif\n";
  close_out oc

let sailpath = Unix.getenv "HOME" ^ "/.opam/default/share/sail/"

let paths_filename = ref ""

let usage_msg = "Usage: riscv_disasm_from_sail -f <path-to-list-of-input-files>"
let arg_spec =
  [
    ( "-f",
      Arg.Set_string paths_filename,
      "Path to a file containing a list of input files, a filename on each line"
    );
  ]
let anon_arg_handler a =
  print_endline ("Unrecognized argument " ^ a ^ ", ignoring...")

let () = Arg.parse arg_spec anon_arg_handler usage_msg

let filepaths = Utils.read_file !paths_filename

let initial_typeenv = Type_check.initial_env

let dummyoptions =
  [
    ("-lem_extern_type", Arg.String (fun _ -> ()), "");
    ("-coq_extern_type", Arg.String (fun _ -> ()), "");
  ]

let _, ast, types, side_effects =
  try Frontend.load_files sailpath dummyoptions initial_typeenv filepaths
  with Reporting.Fatal_error e as ex ->
    Reporting.print_error e;
    raise ex

let ctypedefs, typdefwalker = Gen_clike_typedef.gen_def ast

let ctypedefs_str = Stringify.stringify_typdef ctypedefs

let analysis = Sail_analysis.analyze ast types

let dec = gen_decode_proc (gen_decoder ast_decode_mapping ast analysis)

let compressed_dec =
  gen_decode_proc (gen_decoder ast_compressed_decode_mapping ast analysis)

let dec_str = stringify_decode_procedure dec typdefwalker

let compressed_dec_str =
  stringify_decode_procedure ~c_proc_name:"decode_compressed" compressed_dec
    typdefwalker

let asm = Gen_assembler.gen_asm ast analysis

let asm_str, tables_str = Stringify.stringify_assembler asm typdefwalker

let gen_instr_types_conf =
  Gen_instr_types.read_config "conf/instruction-types/excluded_enums.txt" 

let instr_types = Gen_instr_types.gen_instr_types analysis gen_instr_types_conf

let instr_types_str, instr_types_mapping_str =
  Stringify.stringify_instr_types instr_types typdefwalker

let _ = Gen_operand_info.gen_operand_info ast analysis

let () = write_c_file ast_type_filename ctypedefs_str
let () =
  write_c_file decode_logic_filename dec_str
    ~additional_includes:[ast_type_filename]
let () =
  write_c_file compressed_decode_logic_filename compressed_dec_str
    ~additional_includes:[ast_type_filename]
let () =
  write_c_file assembler_filename asm_str
    ~additional_includes:
      [
        ast_type_filename;
        ast2str_tables_filename;
        "RISCVHelpersAst2Str.h";
        "SStream.h";
      ]

let () =
  write_c_file ast2str_tables_filename tables_str
    ~additional_includes:[ast_type_filename; "SStream.h"]

let () = write_c_file instr_types_filename instr_types_str

let () =
  write_c_file instr_types_mapping_filename instr_types_mapping_str
    ~additional_includes:[instr_types_filename]
