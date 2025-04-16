#ifndef __RISCV_DECODE_HELPERS_H__
#define __RISCV_DECODE_HELPERS_H__

#include "RISCVRVContextHelpers.h"

#include <stdio.h>

#include "RISCVAst.gen.inc"

typedef enum ExtensionType {
  // Integer Multiplication and Division; not Machine!
  RISCV_Ext_M,
  // Single-Precision Floating-Point
  RISCV_Ext_F,
  // Double-Precision Floating-Point
  RISCV_Ext_D,
  // Compressed Instructions
  RISCV_Ext_C,
  // Bit Manipulation
  RISCV_Ext_B,
  // Vector Operations
  RISCV_Ext_V,
  // Supervisor
  RISCV_Ext_S,
  // User
  RISCV_Ext_U,
  // Cache-Block Management Instructions
  RISCV_Ext_Zicbom,
  // Cache-Block Zero Instructions
  RISCV_Ext_Zicboz,
  // Base Counters and Timers
  RISCV_Ext_Zicntr,
  // Integer Conditional Operations
  RISCV_Ext_Zicond,
  // Instruction-Fetch Fence
  RISCV_Ext_Zifencei,
  // Hardware Performance Counters
  RISCV_Ext_Zihpm,
  // May-Be-Operations
  RISCV_Ext_Zimop,
  // Multiplication and Division: Multiplication only
  RISCV_Ext_Zmmul,
  // Atomic Memory Operations
  RISCV_Ext_Zaamo,
  // Byte and Halfword Atomic Memory Operations
  RISCV_Ext_Zabha,
  // Load-Reserved/Store-Conditional Instructions
  RISCV_Ext_Zalrsc,
  // Additional Floating-Point Instructions
  RISCV_Ext_Zfa,
  // Half-Precision Floating-Point
  RISCV_Ext_Zfh,
  // Minimal Half-Precision Floating-Point
  RISCV_Ext_Zfhmin,
  // Floating-Point in Integer Registers (single precision)
  RISCV_Ext_Zfinx,
  // Floating-Point in Integer Registers (double precision)
  RISCV_Ext_Zdinx,
  // Code Size Reduction: compressed instructions excluding floating point loads
  // and stores
  RISCV_Ext_Zca,
  // Code Size Reduction: additional 16-bit aliases
  RISCV_Ext_Zcb,
  // Code Size Reduction: compressed double precision floating point loads and
  // stores
  RISCV_Ext_Zcd,
  // Code Size Reduction: compressed single precision floating point loads and
  // stores
  RISCV_Ext_Zcf,
  // Compressed May-Be-Operations
  RISCV_Ext_Zcmop,
  // Bit Manipulation: Address generation
  RISCV_Ext_Zba,
  // Bit Manipulation: Basic bit-manipulation
  RISCV_Ext_Zbb,
  // Bit Manipulation: Carry-less multiplication
  RISCV_Ext_Zbc,
  // Bit Manipulation: Bit-manipulation for Cryptography
  RISCV_Ext_Zbkb,
  // Bit Manipulation: Carry-less multiplication for Cryptography
  RISCV_Ext_Zbkc,
  // Bit Manipulation: Crossbar permutations
  RISCV_Ext_Zbkx,
  // Bit Manipulation: Single-bit instructions
  RISCV_Ext_Zbs,
  // Scalar & Entropy Source Instructions: NIST Suite: AES Decryption
  RISCV_Ext_Zknd,
  // Scalar & Entropy Source Instructions: NIST Suite: AES Encryption
  RISCV_Ext_Zkne,
  // Scalar & Entropy Source Instructions: NIST Suite: Hash Function
  // Instructions
  RISCV_Ext_Zknh,
  // Scalar & Entropy Source Instructions: Entropy Source ExtensionType
  RISCV_Ext_Zkr,
  // Scalar & Entropy Source Instructions: ShangMi Suite: SM4 Block Cipher
  // Instructions
  RISCV_Ext_Zksed,
  // Scalar & Entropy Source Instructions: ShangMi Suite: SM3 Hash Cipher
  // Instructions
  RISCV_Ext_Zksh,
  // Floating-Point in Integer Registers (half precision)
  RISCV_Ext_Zhinx,
  // Supervisor-mode Timer Interrupts
  RISCV_Ext_Sstc,
  // Fine-Grained Address-Translation Cache Invalidation
  RISCV_Ext_Svinval,
  // Vector Basic Bit-manipulation
  RISCV_Ext_Zvbb,
  // Vector Cryptography Bit-manipulation
  RISCV_Ext_Zvkb,
  // Vector Carryless Multiplication
  RISCV_Ext_Zvbc,
  RISCV_Ext_Zvknhb,
  // NIST Suite: Vector SHA-2 Secure Hash
  RISCV_Ext_Zvknha,
  // Count Overflow and Mode-Based Filtering
  RISCV_Ext_Sscofpmf,
  // NAPOT Translation Contiguity
  RISCV_Ext_Svnapot,
  // Page-Based Memory Types
  RISCV_Ext_Svpbmt,
  // Cycle and Instret Privilege Mode Filtering
  RISCV_Ext_Smcntrpmf
} ExtensionType;

static inline RVBool currentlyEnabled(ExtensionType t, RVContext *ctx) {
  switch (t) {
  // function clause currentlyEnabled(Ext_M) = misa[M] == 0b1
  case RISCV_Ext_M:
    return MISA(M) == 1;
  // function clause currentlyEnabled(Ext_F) =
  //      (misa[F] == 0b1) & (mstatus[FS] != 0b00)
  case RISCV_Ext_F:
    return MISA(F) == 1 && MSTATUS(FS) != 0;
  // function clause currentlyEnabled(Ext_D) =
  //      (misa[D] == 0b1) & (mstatus[FS] != 0b00) & flen >= 64
  case RISCV_Ext_D:
    return MISA(D) == 1 && MSTATUS(FS) != 0 && ctx->flen >= 64;
  // function clause currentlyEnabled(Ext_C) = misa[C] == 0b1
  case RISCV_Ext_C:
    return MISA(C) == 1;
  // function clause currentlyEnabled(Ext_B) = misa[B] == 0b1
  case RISCV_Ext_B:
    return MISA(B) == 1;
  // function clause currentlyEnabled(Ext_V) =
  //      (misa[V] == 0b1) & (mstatus[VS] != 0b00)
  case RISCV_Ext_V:
    return MISA(V) == 1 && MSTATUS(VS) != 0;
  // function clause currentlyEnabled(Ext_S) = misa[S] == 0b1
  case RISCV_Ext_S:
    return MISA(S) == 1;
  // function clause currentlyEnabled(Ext_U) = misa[U] == 0b1
  case RISCV_Ext_U:
    return MISA(U) == 1;
  // function clause currentlyEnabled(Ext_Zicbom) = sys_enable_zicbom()
  case RISCV_Ext_Zicbom:
    return ctx->sys_enable_zicbom();
  // function clause currentlyEnabled(Ext_Zicboz) = sys_enable_zicboz()
  case RISCV_Ext_Zicboz:
    return ctx->sys_enable_zicboz();
  // function clause currentlyEnabled(Ext_Zicntr) = true
  case RISCV_Ext_Zicntr:
    return 1;
  // function clause currentlyEnabled(Ext_Zicond) = true
  case RISCV_Ext_Zicond:
    return 1;
  // function clause currentlyEnabled(Ext_Zifencei) = true
  case RISCV_Ext_Zifencei:
    return 1;
  // function clause currentlyEnabled(Ext_Zihpm) = true
  case RISCV_Ext_Zihpm:
    return 1;
  // function clause currentlyEnabled(Ext_Zmmul) = true
  case RISCV_Ext_Zmmul:
    return 1;
  // function clause currentlyEnabled(Ext_Zaamo) = misa[A] == 0b1
  case RISCV_Ext_Zaamo:
    return MISA(A) == 1;
  // function clause currentlyEnabled(Ext_Zabha) = true
  case RISCV_Ext_Zabha:
    return 1;
  // function clause currentlyEnabled(Ext_Zalrsc) = misa[A] == 0b1
  case RISCV_Ext_Zalrsc:
    return MISA(A) == 1;
  // function clause currentlyEnabled(Ext_Zfa) = true
  case RISCV_Ext_Zfa:
    return 1;
  // function clause currentlyEnabled(Ext_Zfh) =
  //      (misa[F] == 0b1) & (mstatus[FS] != 0b00)
  case RISCV_Ext_Zfh:
    return MISA(F) == 1 && MSTATUS(FS) != 0;
  // function clause currentlyEnabled(Ext_Zfhmin) = currentlyEnabled(Ext_Zfh)
  case RISCV_Ext_Zfhmin:
    return currentlyEnabled(RISCV_Ext_Zfh, ctx);
  // function clause currentlyEnabled(Ext_Zfinx) = sys_enable_zfinx()
  case RISCV_Ext_Zfinx:
    return ctx->sys_enable_zfinx();
  // function clause currentlyEnabled(Ext_Zdinx) =
  //      sys_enable_zfinx() & flen >= 64
  case RISCV_Ext_Zdinx:
    return ctx->sys_enable_zfinx() && ctx->flen >= 64;
  // function clause currentlyEnabled(Ext_Zca) = currentlyEnabled(Ext_C)
  case RISCV_Ext_Zca:
    return currentlyEnabled(RISCV_Ext_C, ctx);
  // function clause currentlyEnabled(Ext_Zcb) =
  //      sys_enable_zcb() & currentlyEnabled(Ext_Zca)
  case RISCV_Ext_Zcb:
    return ctx->sys_enable_zcb() && currentlyEnabled(RISCV_Ext_Zca, ctx);
  // function clause currentlyEnabled(Ext_Zcd) =
  //      currentlyEnabled(Ext_Zca) & currentlyEnabled(Ext_D) &
  //      (xlen == 32 | xlen == 64)
  case RISCV_Ext_Zcd:
    return currentlyEnabled(RISCV_Ext_Zca, ctx) &&
           currentlyEnabled(RISCV_Ext_D, ctx) &&
           (ctx->xlen == 32 || ctx->xlen == 64);
  // function clause currentlyEnabled(Ext_Zcf) =
  //      currentlyEnabled(Ext_Zca) & currentlyEnabled(Ext_F) & xlen == 32
  case RISCV_Ext_Zcf:
    return currentlyEnabled(RISCV_Ext_Zca, ctx) &&
           currentlyEnabled(RISCV_Ext_F, ctx) && ctx->xlen == 32;
  // function clause currentlyEnabled(Ext_Zba) = true | currentlyEnabled(Ext_B)
  case RISCV_Ext_Zba:
    return 1;
  // function clause currentlyEnabled(Ext_Zbb) = true | currentlyEnabled(Ext_B)
  case RISCV_Ext_Zbb:
    return 1;
  // function clause currentlyEnabled(Ext_Zbc) = true
  case RISCV_Ext_Zbc:
    return 1;
  // function clause currentlyEnabled(Ext_Zbkb) = true
  case RISCV_Ext_Zbkb:
    return 1;
  // function clause currentlyEnabled(Ext_Zbkc) = true
  case RISCV_Ext_Zbkc:
    return 1;
  // function clause currentlyEnabled(Ext_Zbkx) = true
  case RISCV_Ext_Zbkx:
    return 1;
  // function clause currentlyEnabled(Ext_Zbs) = true | currentlyEnabled(Ext_B)
  case RISCV_Ext_Zbs:
    return 1;
  // function clause currentlyEnabled(Ext_Zknd) = true
  case RISCV_Ext_Zknd:
    return 1;
  // function clause currentlyEnabled(Ext_Zkne) = true
  case RISCV_Ext_Zkne:
    return 1;
  // function clause currentlyEnabled(Ext_Zknh) = true
  case RISCV_Ext_Zknh:
    return 1;
  // function clause currentlyEnabled(Ext_Zkr) = true
  case RISCV_Ext_Zkr:
    return 1;
  // function clause currentlyEnabled(Ext_Zksed) = true
  case RISCV_Ext_Zksed:
    return 1;
  // function clause currentlyEnabled(Ext_Zksh) = true
  case RISCV_Ext_Zksh:
    return 1;
  // function clause currentlyEnabled(Ext_Zhinx) = sys_enable_zfinx()
  case RISCV_Ext_Zhinx:
    return ctx->sys_enable_zfinx();
  // function clause currentlyEnabled(Ext_Sstc) = sys_enable_sstc()
  case RISCV_Ext_Sstc:
    return ctx->sys_enable_sstx();
  // function clause currentlyEnabled(Ext_Svinval) = sys_enable_svinval()
  case RISCV_Ext_Svinval:
    return ctx->sys_enable_svinval();
  // function clause currentlyEnabled(Ext_Zvbb) = true
  case RISCV_Ext_Zvbb:
    return 1;
  // function clause currentlyEnabled(Ext_Zvkb) = sys_enable_zvkb() |
  // currentlyEnabled(Ext_Zvbb)
  case RISCV_Ext_Zvkb:
    return ctx->sys_enable_zvkb() | currentlyEnabled(RISCV_Ext_Zvbb, ctx);
  // Not supported in the model yet.
  // function clause currentlyEnabled(Ext_Svnapot) = false
  // function clause currentlyEnabled(Ext_Svpbmt) = false
  case RISCV_Ext_Svnapot:
  case RISCV_Ext_Svpbmt:
    return 0;
  // function clause currentlyEnabled(Ext_Sscofpmf) = sys_enable_sscofpmf() &
  // currentlyEnabled(Ext_Zihpm)
  case RISCV_Ext_Sscofpmf:
    return ctx->sys_enable_sscofpmf() && currentlyEnabled(RISCV_Ext_Zihpm, ctx);
  // function clause currentlyEnabled(Ext_Smcntrpmf) = true
  case RISCV_Ext_Smcntrpmf:
    return 1;

  default:
    printf("currentlyEnabled: ERROR! Unknown extension.\n");
    return 0;
  }
}

static inline RVBool not(RVBool b, RVContext *ctx) { return !b; }

static inline uint8_t size_bytes_forwards(enum word_width width,
                                          RVContext *ctx) {
  switch (width) {
  case RISCV_BYTE:
    return 1;
  case RISCV_HALF:
    return 2;
  case RISCV_WORD:
    return 4;
  case RISCV_DOUBLE:
    return 8;
  default:
    printf("size_bytes_forwards: ERROR! Unhandled word_width case");
    return 0xFF;
  }
}

static inline RVBool lrsc_width_valid(enum word_width width, RVContext *ctx) {
  switch (width) {
  case RISCV_WORD:
    return 1;
  case RISCV_DOUBLE:
    return ctx->xlen >= 64;
  default:
    return 0;
  }
}

static inline RVBool amo_width_valid(enum word_width width, RVContext *ctx) {
  switch (width) {
  case RISCV_BYTE:
  case RISCV_HALF:
    return currentlyEnabled(RISCV_Ext_Zabha, ctx);
  case RISCV_WORD:
    return 1;
  case RISCV_DOUBLE:
    return ctx->xlen >= 64;
  default:
    return 0;
  }
}

static inline RVBool haveDoubleFPU(RVContext *ctx) {
  return currentlyEnabled(RISCV_Ext_D, ctx) ||
         currentlyEnabled(RISCV_Ext_Zdinx, ctx);
}

static inline RVBool haveSingleFPU(RVContext *ctx) {
  return currentlyEnabled(RISCV_Ext_F, ctx) ||
         currentlyEnabled(RISCV_Ext_Zfinx, ctx);
}

static inline RVBool haveHalfFPU(RVContext *ctx) {
  return currentlyEnabled(RISCV_Ext_Zfh, ctx) ||
         currentlyEnabled(RISCV_Ext_Zhinx, ctx);
}

static inline RVBool haveHalfMin(RVContext *ctx) {
  return haveHalfFPU(ctx) || currentlyEnabled(RISCV_Ext_Zfhmin, ctx);
}

static inline RVBool in32BitMode(RVContext *ctx) { return ctx->xlen == 32; }

static inline RVBool validDoubleRegsN(uint8_t *regs, RVContext *ctx) {
  if (currentlyEnabled(RISCV_Ext_Zdinx, ctx) && ctx->xlen == 32) {
    for (uint8_t i = 0; regs[i] != 0xff; i++) {
      if (regs[i] & 1) {
        return 0;
      }
    }
  }
  return 1;
}

#define validDoubleRegs(n, ...) validDoubleRegs##n(__VA_ARGS__)

static inline RVBool validDoubleRegs1(uint8_t rs1, RVContext *ctx) {
  uint8_t regs[] = {rs1, 0xFF};
  return validDoubleRegsN(regs, ctx);
}

static inline RVBool validDoubleRegs2(uint8_t rs1, uint8_t rd, RVContext *ctx) {
  uint8_t regs[] = {rs1, rd, 0xFF};
  return validDoubleRegsN(regs, ctx);
}

static inline RVBool validDoubleRegs3(uint8_t rs2, uint8_t rs1, uint8_t rd,
                                      RVContext *ctx) {
  uint8_t regs[] = {rs2, rs1, rd, 0xFF};
  return validDoubleRegsN(regs, ctx);
}

static inline RVBool validDoubleRegs4(uint8_t rs3, uint8_t rs2, uint8_t rs1,
                                      uint8_t rd, RVContext *ctx) {
  uint8_t regs[] = {rs3, rs2, rs1, rd, 0xFF};
  return validDoubleRegsN(regs, ctx);
}

static inline uint32_t get_sew(RVContext *ctx) {
  return 0; // TODO
}

static inline RVBool zvknhab_check_encdec(uint8_t vs2, uint8_t vs1, uint8_t vd,
                                          RVContext *ctx) {
  return 0; // TODO
}

#endif