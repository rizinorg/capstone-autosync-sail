(* The big idea is as follows : represent a Sail decoder as a list of declarative rules
      1- Each rule has several *conditions*, which are assertions on the input bitstream that must
         all evaluate to true in order for the rule to *fire*

      2- If a rule *fires*, then its *consequences* apply

      3- A rule condition can be one of 3 things:
           3-a- Assert: This condition asserts that a bitslice of length n is equal to a constant
           3-b- Bind: This condition always succeeds and binds a slice of length n to a string name
           3-c- Map-Bind: This condition does the following:
                  3-c-i  It matches a bitslice of length n against a number of known constants in a
                         table mapping bitvec constants of length n to enum strings, and
                  3-c-ii If the bitslice matches any of the constants, the corresponding enum string
                         is considered the value of a string name

      As an example, consider the following mapping clause from a RISC-V decoder:

            union clause ast = STORE : (bits(12), regidx, regidx, word_width, bool, bool)

            mapping clause encdec
                  =  STORE(imm7 @ imm5, rs2, rs1, size, false, false)
                <--> imm7 : bits(7) @ rs2 @ rs1 @ 0b0 @ size_enc(size) @ imm5 : bits(5) @ 0b0100011

      The RHS of this clause can be translated into the following list of conditions:
          - First, bind the most-signficiant 7-bit slice of the bitstream and call it imm7

          - Then, bind the next 5-bit slice to rs2
                (( We know that rs2 must be a 5-bit slice from the type declaration of the ast clause,
                   which declares the 2nd element in its argument list to be a regidx,
                   a regidx is defined elsewhere in the RISC-V Sail model as a synonym for a
                   5-bit bitvec. Since rs2 is the second element supplied to the node's
                   constructor in the LHS, that means its type is a regidx, i.e. a 5-bit bitvec ))

          - Then, bind the next 5-bit slice to rs1
          - Then, assert that the next bit is 0
          - Then, check that the next 2 bits match in the following table:
                    BYTE   <-> 0b00,
                    HALF   <-> 0b01,
                    WORD   <-> 0b10,
                    DOUBLE <-> 0b11
            and bind whatever corresponding result to the name 'size'
            (This table happens to contain all possible combinations of 2 bits, therefore a match
             attempt will always succeed. But in general a match attempt against a table can fail.)

          - Then, bind the next 5 bits to imm5
          - Finally, assert that the last 7 bits are 0100011

      In order for the rule to fire, all 7 conditions above has to be satisfied.
      Bind conditions are always satisfied, but assert conditions can fail, and map-bind conditions
      can only fail if the mapping table they reference is not exhaustive.

      4- If a rule didn't fire, the next rule in the decoder (which is nothing but a list of
         rules) is checked.

      5-If a rule fires, its consequences describe what to do to obtain an ast node:

        5-a- Each consequence has a head and a body
        5-b- The head describes which type of ast node is produced
        5-c- The body describes the successive arguments or values used to construct the value
        5-d- Each body element is either a simple assignment of a binding produced in the match,
            or a concat assignment that strings together several bindings to form a bigger value

        In the example used in (3), the consequences of the rule are as follows:
          - The consequence header: Assign_node_type (STORE)
          - The consequence body:
              - Concat the 2 bindings imm7 and imm5 and assign them as the first value of the node
              - Assign the 5-bit value rs2 as the second value of the node
              - Assign the 5-bit value rs1 as the third value of the node
              - Assign the enum value size as the fourth value of the node
              - Assign boolean constants to the remaining values of the node

        Each decoder is effectively a little program in a domain-specific declarative language.
        The language is a rule-based one where each program is simply a table of rules describing
        under what condition a certain action should be taken. *)
open Sail_values

type len = int

type condition =
  | Assert of len * string
  | Bind of len * string
  | Map_bind of len * bv2enum_table * string
  | Struct_map_bind of len * string * bv2struct_table * string

type conditions = condition list

type consequence_head = Assign_node_type of string
type consequence_item = Push of value | Concat_push of value list
type consequences = consequence_head * consequence_item list

type int_operand =
  | Ident of string
  | Funcall of string * string list
  | Number of int
type bv_operand = Bv_concat of string list | Bv_id of string
type guard =
  | Eq_int of int_operand * int_operand
  | Less_eq_int of int_operand * int_operand
  | Less_eq_or_eq of int_operand * int_operand
  | Eq_bit of string * int * bool
  | Eq_bv of string list * string
  | Less_eq_bv of string * string
  | Boolfun of string * string list
  | And of guard * guard
  | Or of guard * guard
  | Not of guard
  | True

type decode_rule = conditions * guard * consequences

type decoder = decode_rule list
