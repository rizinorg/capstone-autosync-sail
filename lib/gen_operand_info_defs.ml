(* The four register files in RISCV and its standard extensions *)
(* The float and double register files overlap, but they should still
   be counted as seperate register files for max clarity of information *)
type regfile = Base | Base_or_Float | Float_or_Double | Vector
type regaccess = Read | Write | Read_and_Write
type reg_operand = Reg of int * regfile * regaccess

type imm_operand = Imm of int

(* The mapping is from ast cases to a list of ("specializations", operand list) pairs
   A "specialization" is either None, or a pair of an argument index and argument value
   (as untyped string)
   This represents the fact that an AST case may have multiple execute clauses,
   each execute clause is specialized on an argument value
   A None specialization represents an unspecialized execute clause, this is the common path *)
type operand_info = {
  registers_info : (string, reg_operand list) Hashtbl.t;
  immediates_info : (string, imm_operand list) Hashtbl.t;
}
