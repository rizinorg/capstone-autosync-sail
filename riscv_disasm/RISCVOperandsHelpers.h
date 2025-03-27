#ifndef __RISCV_OPERANDS_HELPERS_H__
#define __RISCV_OPERANDS_HELPERS_H__

#include "../../include/capstone/capstone.h"
#include "RISCVAst.gen.inc"

// Those macros are hand-written helpers to transform register values
// to the enum values exposed by the capstone module
// The generated disassembler uses 0..31 indices to refer to all registers of
// any type but the capstone module gives all the register flat sequential enum
// values i.e. enum {<enum values for integer registers>, <enum values for float
// regisers, ...} Therefore, we should map the 0..31 range to a corresponding
// range for each register type

// The simplest values are the integer registers, they are
// 1, 2, 3, ..., 32
// the transformation is a straightforward shift by 1
#define AS_GEN_PURPOSE_REG(r) ((r) + 1)

// floats and doubles are more complex
// they come after the values for the general purpose regs, but are interleaved
// so float enum values are 33, 35, 37, ..., 95
#define AS_FLOAT_REG(r) ((2 * (r)) + 33)

// and double enum values are 34, 36, ..., 96
#define AS_DOUBLE_REG(r) ((2 * (r)) + 34)

// vector registers are 97, ..., 128
#define AS_VECTOR_REG(r) ((r) + 97)

// TODO: implement sign extension as in the Sail stdlib
#define SIGN_EXTEND(i) (i)

// Some operands are very hard to infer from the Sail RISCV model
// For example, some instructions are trivial one-liners that return sucess
// without using their register operands at all, so the generator can't infer
// which register file those register indices will index into For another
// example, memory operands is much harder to infer than register operands due
// to all the shenanigans that vectorized LOAD/STORE instructions do with memory
// For those instructions, we simply detect the relevant instruction and
// manually fill its operands
void do_fill_operands(struct ast *tree, cs_riscv_op *ops, uint8_t *op_count) {
  switch (tree->ast_node_type) {
  // first, all the hint instructions
  // hint instructions trivially succeed in the sail model, so
  // the operand inference never gets a chance to see how
  // the register indices are used
  case RISCV_C_ADDI_HINT: {
    ops[0].type = RISCV_OP_REG;
    ops[0].reg = AS_GEN_PURPOSE_REG(tree->ast_node.c_addi_hint);
    *op_count++;
    break;
  }
  case RISCV_C_MV_HINT: {
    ops[0].type = RISCV_OP_REG;
    ops[0].reg = AS_GEN_PURPOSE_REG(tree->ast_node.c_mv_hint);
    *op_count++;
    break;
  }
  case RISCV_C_ADD_HINT: {
    ops[0].type = RISCV_OP_REG;
    ops[0].reg = AS_GEN_PURPOSE_REG(tree->ast_node.c_add_hint);
    *op_count++;
    break;
  }

  case RISCV_C_SLLI_HINT: {
    ops[1].type = RISCV_OP_REG;
    ops[1].reg = AS_GEN_PURPOSE_REG(tree->ast_node.c_slli_hint);
    *op_count++;
    break;
  }

  case RISCV_C_SRLI_HINT: {
    ops[0].type = RISCV_OP_REG;
    ops[0].reg = AS_GEN_PURPOSE_REG(tree->ast_node.c_srli_hint);
    *op_count++;
    break;
  }

  case RISCV_C_SRAI_HINT: {
    ops[0].type = RISCV_OP_REG;
    ops[0].reg = AS_GEN_PURPOSE_REG(tree->ast_node.c_srai_hint);
    *op_count++;
    break;
  }

  case RISCV_FENCE_RESERVED: {
    ops[1].type = RISCV_OP_REG;
    ops[1].reg = AS_GEN_PURPOSE_REG(tree->ast_node.fence_reserved.rs);
    ops[2].type = RISCV_OP_REG;
    ops[2].reg = AS_GEN_PURPOSE_REG(tree->ast_node.fence_reserved.rd);
    *op_count += 2;
    break;
  }

  case RISCV_FENCEI_RESERVED: {
    ops[1].type = RISCV_OP_REG;
    ops[1].reg = AS_GEN_PURPOSE_REG(tree->ast_node.fencei_reserved.rs);
    ops[2].type = RISCV_OP_REG;
    ops[2].reg = AS_GEN_PURPOSE_REG(tree->ast_node.fencei_reserved.rd);
    *op_count += 2;
    break;
  }
  // memory accesses, too complex to automatically infer
  case RISCV_LOADRES: {
    // the read from rs1 is falsely inferred as a register read
    // correct it to a memory read with displacement 0
    ops[0].type = RISCV_OP_MEM;
    ops[0].mem.base = AS_GEN_PURPOSE_REG(tree->ast_node.loadres.rs1);
    ops[0].mem.disp = 0;
    ops[0].access = CS_AC_READ;
    break;
  }
  // same as loadres
  case RISCV_STORECON: {
    ops[1].type = RISCV_OP_MEM;
    ops[1].mem.base = AS_GEN_PURPOSE_REG(tree->ast_node.storecon.rs1);
    ops[1].mem.disp = 0;
    ops[1].access = CS_AC_READ;
    break;
  }

  case RISCV_AMO: {
    ops[1].type = RISCV_OP_MEM;
    ops[1].mem.base = AS_GEN_PURPOSE_REG(tree->ast_node.amo.rs1);
    ops[1].mem.disp = 0;
    ops[1].access = CS_AC_READ;
    break;
  }
  // the immediate and rs1 are falsely inferred as an
  // immediate operand and a register acccess
  // correct them to a memory operand
  case RISCV_LOAD: {
    ops[0].type = RISCV_OP_MEM;
    ops[0].mem.base = AS_GEN_PURPOSE_REG(tree->ast_node.load.rs1);
    ops[0].mem.disp = SIGN_EXTEND(tree->ast_node.load.imm);
    ops[0].access = CS_AC_READ;
    // shift the destination operand
    ops[1] = ops[2];
    *op_count--;
    break;
  }

  case RISCV_STORE: {
    ops[0].type = RISCV_OP_MEM;
    ops[0].mem.base = AS_GEN_PURPOSE_REG(tree->ast_node.store.rs1);
    ops[0].mem.disp = SIGN_EXTEND(tree->ast_node.store.imm);
    ops[0].access = CS_AC_READ;
    *op_count--;
    break;
  }
  }
}

#endif