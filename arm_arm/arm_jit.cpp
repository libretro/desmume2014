/*	Copyright (C) 2006 yopyop
	Copyright (C) 2011 Loren Merritt
	Copyright (C) 2012 DeSmuME team

	This file is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This file is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with the this software.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "types.h"

#ifdef HAVE_JIT

#include <unistd.h>
#include <stddef.h>
#include <stdint.h>

#include "arm_gen.h"
using namespace arm_gen;

#include <android/log.h>
#define LOGE(...) __android_log_print(ANDROID_LOG_ERROR  , "des", __VA_ARGS__) 

#include "instructions.h"
#include "instruction_attributes.h"
#include "MMU.h"
#include "MMU_timing.h"
#include "arm_jit.h"
#include "bios.h"
#include "armcpu.h"

typedef int32_t (*ArmOpCompiler)(iblock*, uint32_t);

static u8 recompile_counts[(1<<26)/16];

DS_ALIGN(4096) uintptr_t compiled_funcs[1<<26] = {0};

///////
// HELPERS
///////
static bool bit(uint32_t value, uint32_t bit)
{
   return value & (1 << bit);
}

static uint32_t bit(uint32_t value, uint32_t first, uint32_t count)
{
   return (value >> first) & ((1 << count) - 1);
}

static uint32_t read_emu_register(iblock* block, reg_t native, reg_t emu, AG_COND cond = AL)
{
   block->ldr(native, 7, mem2::imm(offsetof(armcpu_t, R) + 4 * emu), MEM_NONE, cond);
}

static void write_emu_register(iblock* block, reg_t native, reg_t emu, AG_COND cond = AL)
{
   block->str(native, 7, mem2::imm(offsetof(armcpu_t, R) + 4 * emu), MEM_NONE, cond);
}

static void load_status(iblock* block)
{
   block->ldr(0, 7, mem2::imm(offsetof(armcpu_t, CPSR)));
   block->set_status(0);
}

static void write_status(iblock* block, AG_COND cond = AL)
{
   block->get_status(0, cond);
   block->mov(0, alu2::reg_shift_imm(0, LSR, 24), cond);
   block->strb(0, 7, mem2::imm(offsetof(armcpu_t, CPSR) + 3), MEM_NONE, cond);
}

template <int PROCNUM, bool THUMB>
static void arm_jit_prefetch(iblock* block)
{
   const uint32_t imask = THUMB ? 0xFFFFFFFE : 0xFFFFFFFC;
   const uint32_t isize = THUMB ? 2 : 4;
   const uint32_t iread = THUMB ? (uint32_t)&_MMU_read16<PROCNUM, MMU_AT_CODE>
                                : (uint32_t)&_MMU_read32<PROCNUM, MMU_AT_CODE>;

   block->ldr(0, 7, mem2::imm(offsetof(armcpu_t, next_instruction)));
   block->load_constant(1, imask);
   block->and_(0, alu2::reg(1));

   block->str(0, 7, mem2::imm(offsetof(armcpu_t, instruct_adr)));

   block->add(0, alu2::imm(isize));
   block->str(0, 7, mem2::imm(offsetof(armcpu_t, next_instruction)));

   block->add(0, alu2::imm(isize));
   write_emu_register(block, 0, 15);

   block->sub(0, 0, alu2::imm(isize * 2));
   block->load_constant(1, iread);
   block->blx(1);
   block->str(0, 7, mem2::imm(offsetof(armcpu_t, instruction)));
}

/////////
/// ARM
/////////
static int32_t ARM_OP_ALU(iblock* block, uint32_t opcode)
{
   const reg_t rn = bit(opcode, 16, 4);
   const reg_t rd = bit(opcode, 12, 4);
   const reg_t rm = bit(opcode, 0, 4);
   const reg_t rs = bit(opcode, 8, 4);

   const AG_COND cond = (AG_COND)bit(opcode, 28, 4);
   const AG_ALU_OP op = (AG_ALU_OP)bit(opcode, 20, 5);

   const bool is_immediate = bit(opcode, 25);
   const uint32_t immediate = opcode & 0xFF;
   const uint32_t rotate = bit(opcode, 8, 4);

   const bool is_shift_reg = bit(opcode, 4);
   const uint32_t shift_imm = bit(opcode, 7, 5);
   const AG_ALU_SHIFT shift_type = (AG_ALU_SHIFT)bit(opcode, 5, 2);

   if (rn == 0xF || rd == 0xF || rm == 0xF || rs == 0xF)
      return 1;

   load_status(block);

   block->b("RUN", false, cond);
   block->b("SKIP", false);

   block->set_label("RUN");

   read_emu_register(block, 0, rn);

   if (is_immediate)
   {
      block->alu_op(op, 0, 0, alu2::imm_ror(immediate, rotate << 1));
   }
   else
   {
      read_emu_register(block, 1, rm);

      if (is_shift_reg)
      {
         read_emu_register(block, 2, rs);
         block->alu_op(op, 0, 0, alu2::reg_shift_reg(1, shift_type, 2));
      }
      else
      {
         block->alu_op(op, 0, 0, alu2::reg_shift_imm(1, shift_type, shift_imm));
      }
   }

   if (op < 16 || op >= 24)
   {
      write_emu_register(block, 0, rd);
   }

   if (op & 1)
   {
      write_status(block);
   }

   block->set_label("SKIP");
   block->resolve_label("RUN");
   block->resolve_label("SKIP");

   return 0;
}

#define ARM_ALU_OP_DEF(T) \
   static const ArmOpCompiler ARM_OP_##T##_LSL_IMM = ARM_OP_ALU; \
   static const ArmOpCompiler ARM_OP_##T##_LSL_REG = ARM_OP_ALU; \
   static const ArmOpCompiler ARM_OP_##T##_LSR_IMM = ARM_OP_ALU; \
   static const ArmOpCompiler ARM_OP_##T##_LSR_REG = ARM_OP_ALU; \
   static const ArmOpCompiler ARM_OP_##T##_ASR_IMM = ARM_OP_ALU; \
   static const ArmOpCompiler ARM_OP_##T##_ASR_REG = ARM_OP_ALU; \
   static const ArmOpCompiler ARM_OP_##T##_ROR_IMM = ARM_OP_ALU; \
   static const ArmOpCompiler ARM_OP_##T##_ROR_REG = ARM_OP_ALU; \
   static const ArmOpCompiler ARM_OP_##T##_IMM_VAL = ARM_OP_ALU

ARM_ALU_OP_DEF(AND);
ARM_ALU_OP_DEF(AND_S);
ARM_ALU_OP_DEF(EOR);
ARM_ALU_OP_DEF(EOR_S);
ARM_ALU_OP_DEF(SUB);
ARM_ALU_OP_DEF(SUB_S);
ARM_ALU_OP_DEF(RSB);
ARM_ALU_OP_DEF(RSB_S);
ARM_ALU_OP_DEF(ADD);
ARM_ALU_OP_DEF(ADD_S);
ARM_ALU_OP_DEF(ADC);
ARM_ALU_OP_DEF(ADC_S);
ARM_ALU_OP_DEF(SBC);
ARM_ALU_OP_DEF(SBC_S);
ARM_ALU_OP_DEF(RSC);
ARM_ALU_OP_DEF(RSC_S);
ARM_ALU_OP_DEF(TST);
ARM_ALU_OP_DEF(TEQ);
ARM_ALU_OP_DEF(CMP);
ARM_ALU_OP_DEF(CMN);
ARM_ALU_OP_DEF(ORR);
ARM_ALU_OP_DEF(ORR_S);
ARM_ALU_OP_DEF(MOV);
ARM_ALU_OP_DEF(MOV_S);
ARM_ALU_OP_DEF(BIC);
ARM_ALU_OP_DEF(BIC_S);
ARM_ALU_OP_DEF(MVN);
ARM_ALU_OP_DEF(MVN_S);

#define ARM_MEM_OP_DEF(T, Q) \
   static const ArmOpCompiler ARM_OP_##T##_LSL_##Q = 0; \
   static const ArmOpCompiler ARM_OP_##T##_LSR_##Q = 0; \
   static const ArmOpCompiler ARM_OP_##T##_ASR_##Q = 0; \
   static const ArmOpCompiler ARM_OP_##T##_ROR_##Q = 0

ARM_MEM_OP_DEF(STR_M,   IMM_OFF_POSTIND);
ARM_MEM_OP_DEF(LDR_M,   IMM_OFF_POSTIND);
ARM_MEM_OP_DEF(STRB_M,  IMM_OFF_POSTIND);
ARM_MEM_OP_DEF(LDRB_M,  IMM_OFF_POSTIND);
ARM_MEM_OP_DEF(STR_P,   IMM_OFF_POSTIND);
ARM_MEM_OP_DEF(LDR_P,   IMM_OFF_POSTIND);
ARM_MEM_OP_DEF(STRB_P,  IMM_OFF_POSTIND);
ARM_MEM_OP_DEF(LDRB_P,  IMM_OFF_POSTIND);
ARM_MEM_OP_DEF(STR_M,   IMM_OFF);
ARM_MEM_OP_DEF(LDR_M,   IMM_OFF);
ARM_MEM_OP_DEF(STR_M,   IMM_OFF_PREIND);
ARM_MEM_OP_DEF(LDR_M,   IMM_OFF_PREIND);
ARM_MEM_OP_DEF(STRB_M,  IMM_OFF);
ARM_MEM_OP_DEF(LDRB_M,  IMM_OFF);
ARM_MEM_OP_DEF(STRB_M,  IMM_OFF_PREIND);
ARM_MEM_OP_DEF(LDRB_M,  IMM_OFF_PREIND);
ARM_MEM_OP_DEF(STR_P,   IMM_OFF);
ARM_MEM_OP_DEF(LDR_P,   IMM_OFF);
ARM_MEM_OP_DEF(STR_P,   IMM_OFF_PREIND);
ARM_MEM_OP_DEF(LDR_P,   IMM_OFF_PREIND);
ARM_MEM_OP_DEF(STRB_P,  IMM_OFF);
ARM_MEM_OP_DEF(LDRB_P,  IMM_OFF);
ARM_MEM_OP_DEF(STRB_P,  IMM_OFF_PREIND);
ARM_MEM_OP_DEF(LDRB_P,  IMM_OFF_PREIND);

#define ARM_OP_MUL 0
#define ARM_OP_STRH_POS_INDE_M_REG_OFF 0
#define ARM_OP_LDRD_STRD_POST_INDEX 0
#define ARM_OP_MUL_S 0
#define ARM_OP_LDRH_POS_INDE_M_REG_OFF 0
#define ARM_OP_LDRSB_POS_INDE_M_REG_OFF 0
#define ARM_OP_LDRSH_POS_INDE_M_REG_OFF 0
#define ARM_OP_MLA 0
#define ARM_OP_UND 0
#define ARM_OP_MLA_S 0
#define ARM_OP_STRH_POS_INDE_M_IMM_OFF 0
#define ARM_OP_LDRH_POS_INDE_M_IMM_OFF 0
#define ARM_OP_LDRSB_POS_INDE_M_IMM_OFF 0
#define ARM_OP_LDRSH_POS_INDE_M_IMM_OFF 0
#define ARM_OP_UMULL 0
#define ARM_OP_STRH_POS_INDE_P_REG_OFF 0
#define ARM_OP_UMULL_S 0
#define ARM_OP_LDRH_POS_INDE_P_REG_OFF 0
#define ARM_OP_LDRSB_POS_INDE_P_REG_OFF 0
#define ARM_OP_LDRSH_POS_INDE_P_REG_OFF 0
#define ARM_OP_UMLAL 0
#define ARM_OP_UMLAL_S 0
#define ARM_OP_SMULL 0
#define ARM_OP_STRH_POS_INDE_P_IMM_OFF 0
#define ARM_OP_SMULL_S 0
#define ARM_OP_LDRH_POS_INDE_P_IMM_OFF 0
#define ARM_OP_LDRSB_POS_INDE_P_IMM_OFF 0
#define ARM_OP_LDRSH_POS_INDE_P_IMM_OFF 0
#define ARM_OP_SMLAL 0
#define ARM_OP_SMLAL_S 0
#define ARM_OP_MRS_CPSR 0
#define ARM_OP_QADD 0
#define ARM_OP_SMLA_B_B 0
#define ARM_OP_SWP 0
#define ARM_OP_SMLA_T_B 0
#define ARM_OP_STRH_M_REG_OFF 0
#define ARM_OP_SMLA_B_T 0
#define ARM_OP_LDRD_STRD_OFFSET_PRE_INDEX 0
#define ARM_OP_SMLA_T_T 0
#define ARM_OP_LDRH_M_REG_OFF 0
#define ARM_OP_LDRSB_M_REG_OFF 0
#define ARM_OP_LDRSH_M_REG_OFF 0
#define ARM_OP_MSR_CPSR 0
#define ARM_OP_BX 0
#define ARM_OP_BLX_REG 0
#define ARM_OP_QSUB 0
#define ARM_OP_BKPT 0
#define ARM_OP_SMLAW_B 0
#define ARM_OP_SMULW_B 0
#define ARM_OP_STRH_PRE_INDE_M_REG_OFF 0
#define ARM_OP_SMLAW_T 0
#define ARM_OP_SMULW_T 0
#define ARM_OP_LDRH_PRE_INDE_M_REG_OFF 0
#define ARM_OP_LDRSB_PRE_INDE_M_REG_OFF 0
#define ARM_OP_LDRSH_PRE_INDE_M_REG_OFF 0
#define ARM_OP_MRS_SPSR 0
#define ARM_OP_QDADD 0
#define ARM_OP_SMLAL_B_B 0
#define ARM_OP_SWPB 0
#define ARM_OP_SMLAL_T_B 0
#define ARM_OP_STRH_M_IMM_OFF 0
#define ARM_OP_SMLAL_B_T 0
#define ARM_OP_SMLAL_T_T 0
#define ARM_OP_LDRH_M_IMM_OFF 0
#define ARM_OP_LDRSB_M_IMM_OFF 0
#define ARM_OP_LDRSH_M_IMM_OFF 0
#define ARM_OP_MSR_SPSR 0
#define ARM_OP_CLZ 0
#define ARM_OP_QDSUB 0
#define ARM_OP_SMUL_B_B 0
#define ARM_OP_SMUL_T_B 0
#define ARM_OP_STRH_PRE_INDE_M_IMM_OFF 0
#define ARM_OP_SMUL_B_T 0
#define ARM_OP_SMUL_T_T 0
#define ARM_OP_LDRH_PRE_INDE_M_IMM_OFF 0
#define ARM_OP_LDRSB_PRE_INDE_M_IMM_OFF 0
#define ARM_OP_LDRSH_PRE_INDE_M_IMM_OFF 0
#define ARM_OP_STREX 0
#define ARM_OP_STRH_P_REG_OFF 0
#define ARM_OP_LDREX 0
#define ARM_OP_LDRH_P_REG_OFF 0
#define ARM_OP_LDRSB_P_REG_OFF 0
#define ARM_OP_LDRSH_P_REG_OFF 0
#define ARM_OP_STRH_PRE_INDE_P_REG_OFF 0
#define ARM_OP_LDRH_PRE_INDE_P_REG_OFF 0
#define ARM_OP_LDRSB_PRE_INDE_P_REG_OFF 0
#define ARM_OP_LDRSH_PRE_INDE_P_REG_OFF 0
#define ARM_OP_STRH_P_IMM_OFF 0
#define ARM_OP_LDRH_P_IMM_OFF 0
#define ARM_OP_LDRSB_P_IMM_OFF 0
#define ARM_OP_LDRSH_P_IMM_OFF 0
#define ARM_OP_STRH_PRE_INDE_P_IMM_OFF 0
#define ARM_OP_LDRH_PRE_INDE_P_IMM_OFF 0 
#define ARM_OP_LDRSB_PRE_INDE_P_IMM_OFF 0
#define ARM_OP_LDRSH_PRE_INDE_P_IMM_OFF 0
#define ARM_OP_MSR_CPSR_IMM_VAL 0
#define ARM_OP_MSR_SPSR_IMM_VAL 0
#define ARM_OP_STR_M_IMM_OFF_POSTIND 0
#define ARM_OP_LDR_M_IMM_OFF_POSTIND 0
#define ARM_OP_STRB_M_IMM_OFF_POSTIND 0
#define ARM_OP_LDRB_M_IMM_OFF_POSTIND 0
#define ARM_OP_STR_P_IMM_OFF_POSTIND 0
#define ARM_OP_LDR_P_IMM_OFF_POSTIND 0
#define ARM_OP_STRB_P_IMM_OFF_POSTIND 0
#define ARM_OP_LDRB_P_IMM_OFF_POSTIND 0
#define ARM_OP_STR_M_IMM_OFF 0
#define ARM_OP_LDR_M_IMM_OFF 0
#define ARM_OP_STR_M_IMM_OFF_PREIND 0
#define ARM_OP_LDR_M_IMM_OFF_PREIND 0
#define ARM_OP_STRB_M_IMM_OFF 0
#define ARM_OP_LDRB_M_IMM_OFF 0
#define ARM_OP_STRB_M_IMM_OFF_PREIND 0
#define ARM_OP_LDRB_M_IMM_OFF_PREIND 0
#define ARM_OP_STR_P_IMM_OFF 0
#define ARM_OP_LDR_P_IMM_OFF 0
#define ARM_OP_STR_P_IMM_OFF_PREIND 0
#define ARM_OP_LDR_P_IMM_OFF_PREIND 0
#define ARM_OP_STRB_P_IMM_OFF 0
#define ARM_OP_LDRB_P_IMM_OFF 0
#define ARM_OP_STRB_P_IMM_OFF_PREIND 0
#define ARM_OP_LDRB_P_IMM_OFF_PREIND 0
#define ARM_OP_STMDA 0
#define ARM_OP_LDMDA 0
#define ARM_OP_STMDA_W 0
#define ARM_OP_LDMDA_W 0
#define ARM_OP_STMDA2 0
#define ARM_OP_LDMDA2 0
#define ARM_OP_STMDA2_W 0
#define ARM_OP_LDMDA2_W 0
#define ARM_OP_STMIA 0
#define ARM_OP_LDMIA 0
#define ARM_OP_STMIA_W 0
#define ARM_OP_LDMIA_W 0
#define ARM_OP_STMIA2 0
#define ARM_OP_LDMIA2 0
#define ARM_OP_STMIA2_W 0
#define ARM_OP_LDMIA2_W 0
#define ARM_OP_STMDB 0
#define ARM_OP_LDMDB 0
#define ARM_OP_STMDB_W 0
#define ARM_OP_LDMDB_W 0
#define ARM_OP_STMDB2 0
#define ARM_OP_LDMDB2 0
#define ARM_OP_STMDB2_W 0
#define ARM_OP_LDMDB2_W 0
#define ARM_OP_STMIB 0
#define ARM_OP_LDMIB 0
#define ARM_OP_STMIB_W 0
#define ARM_OP_LDMIB_W 0
#define ARM_OP_STMIB2 0
#define ARM_OP_LDMIB2 0
#define ARM_OP_STMIB2_W 0
#define ARM_OP_LDMIB2_W 0
#define ARM_OP_B 0
#define ARM_OP_BL 0
#define ARM_OP_STC_OPTION 0
#define ARM_OP_LDC_OPTION 0
#define ARM_OP_STC_M_POSTIND 0
#define ARM_OP_LDC_M_POSTIND 0
#define ARM_OP_STC_P_POSTIND 0
#define ARM_OP_LDC_P_POSTIND 0
#define ARM_OP_STC_M_IMM_OFF 0
#define ARM_OP_LDC_M_IMM_OFF 0
#define ARM_OP_STC_M_PREIND 0
#define ARM_OP_LDC_M_PREIND 0
#define ARM_OP_STC_P_IMM_OFF 0
#define ARM_OP_LDC_P_IMM_OFF 0
#define ARM_OP_STC_P_PREIND 0
#define ARM_OP_LDC_P_PREIND 0
#define ARM_OP_CDP 0
#define ARM_OP_MCR 0
#define ARM_OP_MRC 0
#define ARM_OP_SWI 0

static const ArmOpCompiler arm_instruction_compilers[4096] = {
#define TABDECL(x) ARM_##x
#include "instruction_tabdef.inc"
#undef TABDECL
};

template<int PROCNUM>
static void arm_arm_gen_code_for(iblock* block, uint32_t address, uint32_t opcode)
{
   // NOTE: Expected register usage
   // R0 - R3 = Used and clobbered internally
   // R5 = Pointer to armcpu_exec function
   // R6 = Cycle counter
   // R7 = Pointer to ARMPROC

   ArmOpCompiler compiler = arm_instruction_compilers[INSTRUCTION_INDEX(opcode)];
   if (compiler && (compiler(block, opcode) == 0))
   {
      // HACK: Use real cycles
      block->add(6, alu2::imm(4));
      arm_jit_prefetch<PROCNUM, false>(block);
   }
   else
   {
      block->blx(5);
      block->add(6, 6, alu2::reg(0));
   }
}


////////
// THUMB
////////
static int32_t THUMB_OP_SHIFT(iblock* block, uint32_t opcode)
{
   const uint32_t rd = bit(opcode, 0, 3);
   const uint32_t rs = bit(opcode, 3, 3);
   const uint32_t imm = bit(opcode, 6, 5);
   const AG_ALU_SHIFT op = (AG_ALU_SHIFT)bit(opcode, 11, 2);

   load_status(block);

   read_emu_register(block, 0, rs);
   block->movs(0, alu2::reg_shift_imm(0, op, imm));
   write_emu_register(block, 0, rd);

   write_status(block);

   return 0;
}

static int32_t THUMB_OP_ADDSUB_REGIMM(iblock* block, uint32_t opcode)
{
   const uint32_t rd = bit(opcode, 0, 3);
   const uint32_t rs = bit(opcode, 3, 3);
   const AG_ALU_OP op = bit(opcode, 9) ? SUBS : ADDS;
   const bool arg_type = bit(opcode, 10);
   const uint32_t arg = bit(opcode, 6, 3);

   load_status(block);
   read_emu_register(block, 0, rs);

   if (arg_type) // Immediate
   {
      block->alu_op(op, 0, 0, alu2::imm(arg));
   }
   else
   {
      read_emu_register(block, 1, arg);
      block->alu_op(op, 0, 0, alu2::reg(1));
   }

   write_emu_register(block, 0, rd);
   write_status(block);

   return 0;
}

static int32_t THUMB_OP_MCAS_IMM8(iblock* block, uint32_t opcode)
{
   const reg_t rd = bit(opcode, 8, 3);
   const uint32_t op = bit(opcode, 11, 2);
   const uint32_t imm = bit(opcode, 0, 8);

   load_status(block);
   read_emu_register(block, 0, rd);
   
   switch (op)
   {
      case 0: block->alu_op(MOVS, 0, 0, alu2::imm(imm)); break;
      case 1: block->alu_op(CMP , 0, 0, alu2::imm(imm)); break;
      case 2: block->alu_op(ADDS, 0, 0, alu2::imm(imm)); break;
      case 3: block->alu_op(SUBS, 0, 0, alu2::imm(imm)); break;
   }

   if (op != 1) // Don't keep the result of a CMP instruction
   {
      write_emu_register(block, 0, rd);
   }

   write_status(block);

   return 0;
}

static int32_t THUMB_OP_ALU(iblock* block, uint32_t opcode)
{
   const uint32_t rd = bit(opcode, 0, 3);
   const uint32_t rs = bit(opcode, 3, 3);
   const uint32_t op = bit(opcode, 6, 4);
   bool need_writeback = false;

   if (op == 13) // TODO: The MULS is interpreted for now
   {
      return 1;
   }

   load_status(block);
   read_emu_register(block, 0, rd);
   read_emu_register(block, 1, rs);

   switch (op)
   {
      case  0: block->ands(0, alu2::reg(1)); break;
      case  1: block->eors(0, alu2::reg(1)); break;
      case  5: block->adcs(0, alu2::reg(1)); break;
      case  6: block->sbcs(0, alu2::reg(1)); break;
      case  8: block->tst (0, alu2::reg(1)); break;
      case 10: block->cmp (0, alu2::reg(1)); break;
      case 11: block->cmn (0, alu2::reg(1)); break;
      case 12: block->orrs(0, alu2::reg(1)); break;
      case 14: block->bics(0, alu2::reg(1)); break;
      case 15: block->mvns(0, alu2::reg(1)); break;

      case  2: block->movs(0, alu2::reg_shift_reg(0, LSL, 1)); break;
      case  3: block->movs(0, alu2::reg_shift_reg(0, LSR, 1)); break;
      case  4: block->movs(0, alu2::reg_shift_reg(0, ASR, 1)); break;
      case  7: block->movs(0, alu2::reg_shift_reg(0, arm_gen::ROR, 1)); break;

      case  9: block->rsbs(0, 1, alu2::imm(0)); break;

      case 13: abort(); break;// TODO: MULS
   }

   static const bool op_wb[16] = { 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1 };
   if (op_wb[op])
   {
      write_emu_register(block, 0, rd);
   }

   write_status(block);

   return 0;
}

#define THUMB_OP_INTERPRET       0
#define THUMB_OP_UND_THUMB       THUMB_OP_INTERPRET

#define THUMB_OP_LSL             THUMB_OP_SHIFT
#define THUMB_OP_LSL_0           THUMB_OP_SHIFT
#define THUMB_OP_LSR             THUMB_OP_SHIFT
#define THUMB_OP_LSR_0           THUMB_OP_SHIFT
#define THUMB_OP_ASR             THUMB_OP_SHIFT
#define THUMB_OP_ASR_0           THUMB_OP_SHIFT

#define THUMB_OP_ADD_REG         THUMB_OP_ADDSUB_REGIMM
#define THUMB_OP_SUB_REG         THUMB_OP_ADDSUB_REGIMM
#define THUMB_OP_ADD_IMM3        THUMB_OP_ADDSUB_REGIMM
#define THUMB_OP_SUB_IMM3        THUMB_OP_ADDSUB_REGIMM

#define THUMB_OP_MOV_IMM8        THUMB_OP_MCAS_IMM8
#define THUMB_OP_CMP_IMM8        THUMB_OP_MCAS_IMM8
#define THUMB_OP_ADD_IMM8        THUMB_OP_MCAS_IMM8
#define THUMB_OP_SUB_IMM8        THUMB_OP_MCAS_IMM8

#define THUMB_OP_AND             THUMB_OP_ALU
#define THUMB_OP_EOR             THUMB_OP_ALU
#define THUMB_OP_LSL_REG         THUMB_OP_ALU
#define THUMB_OP_LSR_REG         THUMB_OP_ALU
#define THUMB_OP_ASR_REG         THUMB_OP_ALU
#define THUMB_OP_ADC_REG         THUMB_OP_ALU
#define THUMB_OP_SBC_REG         THUMB_OP_ALU
#define THUMB_OP_ROR_REG         THUMB_OP_ALU
#define THUMB_OP_TST             THUMB_OP_ALU
#define THUMB_OP_NEG             THUMB_OP_ALU
#define THUMB_OP_CMP             THUMB_OP_ALU
#define THUMB_OP_CMN             THUMB_OP_ALU
#define THUMB_OP_ORR             THUMB_OP_ALU
#define THUMB_OP_MUL_REG         THUMB_OP_INTERPRET
#define THUMB_OP_BIC             THUMB_OP_ALU
#define THUMB_OP_MVN             THUMB_OP_ALU

// UNDEFINED OPS
#define THUMB_OP_ADD_SPE         THUMB_OP_INTERPRET
#define THUMB_OP_CMP_SPE         THUMB_OP_INTERPRET
#define THUMB_OP_MOV_SPE         THUMB_OP_INTERPRET
#define THUMB_OP_BX_THUMB        THUMB_OP_INTERPRET
#define THUMB_OP_BLX_THUMB       THUMB_OP_INTERPRET

#define THUMB_OP_LDR_PCREL       THUMB_OP_INTERPRET

#define THUMB_OP_STR_REG_OFF     THUMB_OP_INTERPRET
#define THUMB_OP_STRH_REG_OFF    THUMB_OP_INTERPRET
#define THUMB_OP_STRB_REG_OFF    THUMB_OP_INTERPRET
#define THUMB_OP_LDRSB_REG_OFF   THUMB_OP_INTERPRET
#define THUMB_OP_LDR_REG_OFF     THUMB_OP_INTERPRET
#define THUMB_OP_LDRH_REG_OFF    THUMB_OP_INTERPRET
#define THUMB_OP_LDRB_REG_OFF    THUMB_OP_INTERPRET
#define THUMB_OP_LDRSH_REG_OFF   THUMB_OP_INTERPRET
#define THUMB_OP_STR_IMM_OFF     THUMB_OP_INTERPRET
#define THUMB_OP_LDR_IMM_OFF     THUMB_OP_INTERPRET
#define THUMB_OP_STRB_IMM_OFF    THUMB_OP_INTERPRET
#define THUMB_OP_LDRB_IMM_OFF    THUMB_OP_INTERPRET
#define THUMB_OP_STRH_IMM_OFF    THUMB_OP_INTERPRET
#define THUMB_OP_LDRH_IMM_OFF    THUMB_OP_INTERPRET
#define THUMB_OP_STR_SPREL       THUMB_OP_INTERPRET
#define THUMB_OP_LDR_SPREL       THUMB_OP_INTERPRET
#define THUMB_OP_ADD_2PC         THUMB_OP_INTERPRET
#define THUMB_OP_ADD_2SP         THUMB_OP_INTERPRET
#define THUMB_OP_ADJUST_P_SP     THUMB_OP_INTERPRET
#define THUMB_OP_ADJUST_M_SP     THUMB_OP_INTERPRET
#define THUMB_OP_PUSH            THUMB_OP_INTERPRET
#define THUMB_OP_PUSH_LR         THUMB_OP_INTERPRET
#define THUMB_OP_POP             THUMB_OP_INTERPRET
#define THUMB_OP_POP_PC          THUMB_OP_INTERPRET
#define THUMB_OP_BKPT_THUMB      THUMB_OP_INTERPRET
#define THUMB_OP_STMIA_THUMB     THUMB_OP_INTERPRET
#define THUMB_OP_LDMIA_THUMB     THUMB_OP_INTERPRET
#define THUMB_OP_B_COND          THUMB_OP_INTERPRET
#define THUMB_OP_SWI_THUMB       THUMB_OP_INTERPRET
#define THUMB_OP_B_UNCOND        THUMB_OP_INTERPRET
#define THUMB_OP_BLX             THUMB_OP_INTERPRET
#define THUMB_OP_BL_10           THUMB_OP_INTERPRET
#define THUMB_OP_BL_11           THUMB_OP_INTERPRET

static const ArmOpCompiler thumb_instruction_compilers[1024] = {
#define TABDECL(x) THUMB_##x
#include "thumb_tabdef.inc"
#undef TABDECL
};

template<int PROCNUM>
static void arm_thumb_gen_code_for(iblock* block, uint32_t address, uint32_t opcode)
{
   assert(opcode < 0x10000);

   // NOTE: Expected register usage
   // R0 - R3 = Used and clobbered internally
   // R5 = Pointer to armcpu_exec function
   // R6 = Cycle counter
   // R7 = Pointer to ARMPROC

   ArmOpCompiler compiler = thumb_instruction_compilers[opcode >> 6];
   if (compiler)
   {
      compiler(block, opcode);

      // HACK: Use real cycles
      block->add(6, alu2::imm(4));
      arm_jit_prefetch<PROCNUM, true>(block);
   }
   else
   {
      block->blx(5);
      block->add(6, 6, alu2::reg(0));
   }
}



// ============================================================================================= IMM

//-----------------------------------------------------------------------------
//   Compiler
//-----------------------------------------------------------------------------

static u32 instr_attributes(bool thumb, u32 opcode)
{
   return thumb ? thumb_attributes[opcode>>6]
                : instruction_attributes[INSTRUCTION_INDEX(opcode)];
}

static bool instr_is_branch(bool thumb, u32 opcode)
{
   u32 x = instr_attributes(thumb, opcode);
   if(thumb)
      return (x & BRANCH_ALWAYS)
          || ((x & BRANCH_POS0) && ((opcode&7) | ((opcode>>4)&8)) == 15)
          || (x & BRANCH_SWI)
          || (x & JIT_BYPASS);
   else
      return (x & BRANCH_ALWAYS)
          || ((x & BRANCH_POS12) && REG_POS(opcode,12) == 15)
          || ((x & BRANCH_LDM) && BIT15(opcode))
          || (x & BRANCH_SWI)
          || (x & JIT_BYPASS);
}

template<int PROCNUM>
static ArmOpCompiled compile_basicblock(iblock* block)
{
   const bool thumb = ARMPROC.CPSR.bits.T == 1;
   const u32 base = ARMPROC.instruct_adr;
   const u32 isize = thumb ? 2 : 4;

   block->push(0x40F0);                          // push {r4-r7, r14}

   block->load_constant(7, (uint32_t)&ARMPROC);  // r7 = cpu_state
   block->load_constant(6, 0);                   // r6 = cycle count
   block->load_constant(5, (uint32_t)&armcpu_exec<PROCNUM>);

   for (uint32_t i = 0; i < CommonSettings.jit_max_block_size; i ++)
   {
      uint32_t pc = base + i * isize;
      uint32_t opcode = thumb ? _MMU_read16<PROCNUM, MMU_AT_CODE>(pc) : _MMU_read32<PROCNUM, MMU_AT_CODE>(pc);

      if (thumb)
      {
         arm_thumb_gen_code_for<PROCNUM>(block, pc, opcode);
      }
      else
      {
         arm_arm_gen_code_for<PROCNUM>(block, pc, opcode);
      }

      if (instr_is_branch(thumb, opcode))
         break;
   }

   block->mov(0, alu2::reg(6));

   block->pop(0x40F0); // pop {r4-r7, r14}
   block->bx(14);
   block->cache_flush();

   JIT_COMPILED_FUNC(base, PROCNUM) = (uintptr_t)block->fn_pointer();
   return (ArmOpCompiled)block->fn_pointer();
}

template<int PROCNUM> u32 arm_jit_compile()
{
   u32 adr = ARMPROC.instruct_adr;
   u32 mask_adr = (adr & 0x07FFFFFE) >> 4;
// if(((recompile_counts[mask_adr >> 1] >> 4*(mask_adr & 1)) & 0xF) > 8)
// {
//    ArmOpCompiled f = op_decode[PROCNUM][cpu->CPSR.bits.T];
//    JIT_COMPILED_FUNC(adr, PROCNUM) = (uintptr_t)f;
//    return f();
// }
   recompile_counts[mask_adr >> 1] = 1;

   iblock* block = get_empty_block(adr, PROCNUM);
   if (!block)
   {
      arm_jit_reset(true);
      block = get_empty_block(adr, PROCNUM);
   }

   return compile_basicblock<PROCNUM>(block)();
}

template u32 arm_jit_compile<0>();
template u32 arm_jit_compile<1>();

void arm_jit_reset(bool enable)
{
   if (enable)
   {
      for(int i=0; i<sizeof(recompile_counts)/8; i++)
         if(((u64*)recompile_counts)[i])
         {
            ((u64*)recompile_counts)[i] = 0;
            memset(compiled_funcs+128*i, 0, 128*sizeof(*compiled_funcs));
         }
   }

   arm_gen::init();
}

void arm_jit_close()
{
}
#endif // HAVE_JIT
