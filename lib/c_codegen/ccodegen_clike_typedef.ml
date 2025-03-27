open Capstone_autosync_sail

open Clike_typedef
open Constants

let stringify_clike_builin name builtin =
  match builtin with
  | Clike_bitfield size ->
      let pow2_rounded_size =
        if size < 8 then 8
        else if size < 16 then 16
        else if size < 32 then 32
        else 64
      in
      "uint"
      ^ string_of_int pow2_rounded_size
      ^ "_t " ^ name ^ " /* bits : " ^ string_of_int size ^ " */;"
  | Clike_byte -> "uint8_t " ^ name ^ ";"
  | Clike_word -> "uint16_t " ^ name ^ ";"
  | Clike_dword -> "uint32_t " ^ name ^ ";"
  | Clike_qword -> "uint64_t " ^ name ^ ";"

let rec stringify_clike_typedef clike_typdef =
  match clike_typdef with
  | Clike_enum (typname, name, constants) ->
      let enum_ctype = if typname = "" then "uint64_t" else typname in
      enum_ctype ^ " " ^ name ^ ";\n"
  | Clike_struct (typname, name, members) ->
      let members_as_str = List.map stringify_clike_typedef members in
      "struct " ^ typname ^ " {\n"
      ^ String.concat "" members_as_str
      ^ "\n} " ^ name ^ ";"
  | Clike_union (typname, name, members) ->
      let members_as_str = List.map stringify_clike_typedef members in
      "union " ^ typname ^ " {\n"
      ^ String.concat "" members_as_str
      ^ "\n} " ^ name ^ ";"
  | Clike_builtin (name, bitvec) -> stringify_clike_builin name bitvec
  | Clike_void -> ""
  | Clike_typename (typname, name) -> typname ^ " " ^ name ^ ";"

let rec stringify_enum_only_typedef clike_typdef =
  match clike_typdef with
  | Clike_enum (typname, name, constants) ->
      "enum " ^ typname ^ " {\n" ^ String.concat ",\n" constants ^ "\n};\n"
  | Clike_struct (typname, name, members) ->
      let members_as_str = List.map stringify_enum_only_typedef members in
      String.concat "" members_as_str
  | Clike_union (typname, name, members) ->
      let members_as_str = List.map stringify_enum_only_typedef members in
      String.concat "" members_as_str
  | Clike_builtin (name, bitvec) -> ""
  | Clike_void -> ""
  | Clike_typename (typname, name) -> ""

let stringify_typdef typdef =
  "enum {" ^ identifier_prefix ^ "false = 0, " ^ identifier_prefix
  ^ "true = 1};\n"
  ^ stringify_enum_only_typedef typdef
  ^ stringify_clike_typedef typdef
