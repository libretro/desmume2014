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
#include "reg_manager.h"
using namespace arm_gen;

#include "instructions.h"
#include "instruction_attributes.h"
#include "MMU.h"
#include "MMU_timing.h"
#include "arm_jit.h"
#include "bios.h"
#include "armcpu.h"

enum OP_RESULT { OPR_CONTINUE, OPR_INTERPRET, OPR_BRANCHED };
typedef OP_RESULT (*ArmOpCompiler)(uint32_t pc, uint32_t opcode);

static const uint32_t INSTRUCTION_COUNT = 0x400000;
static code_pool* block;
static register_manager* regman;
static u8 recompile_counts[(1<<26)/16];

DS_ALIGN(4096) uintptr_t compiled_funcs[1<<26] = {0};

const reg_t RCPU = 4;
const reg_t RCYC = 5;
static uint32_t block_procnum;

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

static uint32_t bit_write(uint32_t value, uint32_t first, uint32_t count, uint32_t insert)
{
   uint32_t result = value & ~(((1 << count) - 1) << first);
   return result | (insert << first);
}

static void load_status()
{
   block->ldr(0, RCPU, mem2::imm(offsetof(armcpu_t, CPSR)));
   block->set_status(0);
}

static void write_status(AG_COND cond = AL)
{
   block->get_status(0, cond);
   block->mov(0, alu2::reg_shift_imm(0, LSR, 24), cond);
   block->strb(0, RCPU, mem2::imm(offsetof(armcpu_t, CPSR) + 3), MEM_NONE, cond);
}

template <int PROCNUM>
static void arm_jit_prefetch(uint32_t pc, uint32_t opcode, bool thumb)
{
   const uint32_t imask = thumb ? 0xFFFFFFFE : 0xFFFFFFFC;
   const uint32_t isize = thumb ? 2 : 4;

   block->load_constant(0, pc & imask);
   block->str(0, RCPU, mem2::imm(offsetof(armcpu_t, instruct_adr)));

   block->add(0, alu2::imm(isize));
   block->str(0, RCPU, mem2::imm(offsetof(armcpu_t, next_instruction)));

   block->add(0, alu2::imm(isize));
   block->str(0, RCPU, mem2::imm(offsetof(armcpu_t, R) + 4 * 15));

   block->load_constant(0, opcode);
   block->str(0, RCPU, mem2::imm(offsetof(armcpu_t, instruction)));
}

/////////
/// ARM
/////////
template <int AT16, int AT12, int AT8, int AT0, bool S, uint32_t CYC>
static OP_RESULT ARM_OP_PATCH(uint32_t pc, uint32_t opcode)
{
   const reg_t at16 = bit(opcode, 16, 4);
   const reg_t at12 = bit(opcode, 12, 4);
   const reg_t at8  = bit(opcode, 8, 4);
   const reg_t at0  = bit(opcode, 0, 4);

   if ((AT16 && (at16 == 0xF)) || (AT12 && (at12 == 0xF)) || (AT8 && (at8 == 0xF)) || (AT0 && (at0 == 0xF)))
      return OPR_INTERPRET;

   const reg_t nat16 = (AT16) ? regman->get(at16) : at16;
   const reg_t nat12 = (AT12) ? regman->get(at12) : at12;
   const reg_t nat8  = (AT8 ) ? regman->get(at8 ) : at8 ;
   const reg_t nat0  = (AT0 ) ? regman->get(at0 ) : at0 ;

   opcode = bit_write(opcode, 16, 4, nat16);
   opcode = bit_write(opcode, 12, 4, nat12);
   opcode = bit_write(opcode,  8, 4, nat8 );
   opcode = bit_write(opcode,  0, 4, nat0 );

   load_status();
   block->insert_raw_instruction(opcode);
   if (S) write_status();

   if (AT16 & 2) regman->mark_dirty(nat16);
   if (AT12 & 2) regman->mark_dirty(nat12);
   if (AT8  & 2) regman->mark_dirty(nat8 );
   if (AT0  & 2) regman->mark_dirty(nat0 );

   block->add(RCYC, alu2::imm(CYC));

   return OPR_CONTINUE;
}

#define ARM_ALU_OP_DEF(T, D, N, S) \
   static const ArmOpCompiler ARM_OP_##T##_LSL_IMM = ARM_OP_PATCH<N, D, 0, 1, S, 1>; \
   static const ArmOpCompiler ARM_OP_##T##_LSL_REG = ARM_OP_PATCH<N, D, 1, 1, S, 2>; \
   static const ArmOpCompiler ARM_OP_##T##_LSR_IMM = ARM_OP_PATCH<N, D, 0, 1, S, 1>; \
   static const ArmOpCompiler ARM_OP_##T##_LSR_REG = ARM_OP_PATCH<N, D, 1, 1, S, 2>; \
   static const ArmOpCompiler ARM_OP_##T##_ASR_IMM = ARM_OP_PATCH<N, D, 0, 1, S, 1>; \
   static const ArmOpCompiler ARM_OP_##T##_ASR_REG = ARM_OP_PATCH<N, D, 1, 1, S, 2>; \
   static const ArmOpCompiler ARM_OP_##T##_ROR_IMM = ARM_OP_PATCH<N, D, 0, 1, S, 1>; \
   static const ArmOpCompiler ARM_OP_##T##_ROR_REG = ARM_OP_PATCH<N, D, 1, 1, S, 2>; \
   static const ArmOpCompiler ARM_OP_##T##_IMM_VAL = ARM_OP_PATCH<N, D, 0, 0, S, 1>

ARM_ALU_OP_DEF(AND  , 2, 1, false);
ARM_ALU_OP_DEF(AND_S, 2, 1, true);
ARM_ALU_OP_DEF(EOR  , 2, 1, false);
ARM_ALU_OP_DEF(EOR_S, 2, 1, true);
ARM_ALU_OP_DEF(SUB  , 2, 1, false);
ARM_ALU_OP_DEF(SUB_S, 2, 1, true);
ARM_ALU_OP_DEF(RSB  , 2, 1, false);
ARM_ALU_OP_DEF(RSB_S, 2, 1, true);
ARM_ALU_OP_DEF(ADD  , 2, 1, false);
ARM_ALU_OP_DEF(ADD_S, 2, 1, true);
ARM_ALU_OP_DEF(ADC  , 2, 1, false);
ARM_ALU_OP_DEF(ADC_S, 2, 1, true);
ARM_ALU_OP_DEF(SBC  , 2, 1, false);
ARM_ALU_OP_DEF(SBC_S, 2, 1, true);
ARM_ALU_OP_DEF(RSC  , 2, 1, false);
ARM_ALU_OP_DEF(RSC_S, 2, 1, true);
ARM_ALU_OP_DEF(TST  , 0, 1, true);
ARM_ALU_OP_DEF(TEQ  , 0, 1, true);
ARM_ALU_OP_DEF(CMP  , 0, 1, true);
ARM_ALU_OP_DEF(CMN  , 0, 1, true);
ARM_ALU_OP_DEF(ORR  , 2, 1, false);
ARM_ALU_OP_DEF(ORR_S, 2, 1, true);
ARM_ALU_OP_DEF(MOV  , 2, 0, false);
ARM_ALU_OP_DEF(MOV_S, 2, 0, true);
ARM_ALU_OP_DEF(BIC  , 2, 1, false);
ARM_ALU_OP_DEF(BIC_S, 2, 1, true);
ARM_ALU_OP_DEF(MVN  , 2, 0, false);
ARM_ALU_OP_DEF(MVN_S, 2, 0, true);

// HACK: multiply cycles are wrong
#define ARM_OP_MUL         ARM_OP_PATCH<2, 0, 1, 1, false, 3>
#define ARM_OP_MUL_S       ARM_OP_PATCH<2, 0, 1, 1, true, 3>
#define ARM_OP_MLA         ARM_OP_PATCH<2, 1, 1, 1, false, 4>
#define ARM_OP_MLA_S       ARM_OP_PATCH<2, 1, 1, 1, true, 4>
#define ARM_OP_UMULL       ARM_OP_PATCH<2, 2, 1, 1, false, 4>
#define ARM_OP_UMULL_S     ARM_OP_PATCH<2, 2, 1, 1, true, 4>
#define ARM_OP_UMLAL       ARM_OP_PATCH<3, 3, 1, 1, false, 5>
#define ARM_OP_UMLAL_S     ARM_OP_PATCH<3, 3, 1, 1, true, 5>
#define ARM_OP_SMULL       ARM_OP_PATCH<2, 2, 1, 1, false, 4>
#define ARM_OP_SMULL_S     ARM_OP_PATCH<2, 2, 1, 1, true, 4>
#define ARM_OP_SMLAL       ARM_OP_PATCH<3, 3, 1, 1, false, 5>
#define ARM_OP_SMLAL_S     ARM_OP_PATCH<3, 3, 1, 1, true, 5>

#define ARM_OP_SMUL_B_B    ARM_OP_PATCH<2, 0, 1, 1, true, 2>
#define ARM_OP_SMUL_T_B    ARM_OP_PATCH<2, 0, 1, 1, true, 2>
#define ARM_OP_SMUL_B_T    ARM_OP_PATCH<2, 0, 1, 1, true, 2>
#define ARM_OP_SMUL_T_T    ARM_OP_PATCH<2, 0, 1, 1, true, 2>

#define ARM_OP_SMLA_B_B    ARM_OP_PATCH<2, 1, 1, 1, true, 2>
#define ARM_OP_SMLA_T_B    ARM_OP_PATCH<2, 1, 1, 1, true, 2>
#define ARM_OP_SMLA_B_T    ARM_OP_PATCH<2, 1, 1, 1, true, 2>
#define ARM_OP_SMLA_T_T    ARM_OP_PATCH<2, 1, 1, 1, true, 2>

#define ARM_OP_SMULW_B     ARM_OP_PATCH<2, 0, 1, 1, true, 2>
#define ARM_OP_SMULW_T     ARM_OP_PATCH<2, 0, 1, 1, true, 2>
#define ARM_OP_SMLAW_B     ARM_OP_PATCH<2, 1, 1, 1, true, 2>
#define ARM_OP_SMLAW_T     ARM_OP_PATCH<2, 1, 1, 1, true, 2>

#define ARM_OP_SMLAL_B_B   ARM_OP_PATCH<3, 3, 1, 1, true, 2>
#define ARM_OP_SMLAL_T_B   ARM_OP_PATCH<3, 3, 1, 1, true, 2>
#define ARM_OP_SMLAL_B_T   ARM_OP_PATCH<3, 3, 1, 1, true, 2>
#define ARM_OP_SMLAL_T_T   ARM_OP_PATCH<3, 3, 1, 1, true, 2>

#define ARM_OP_QADD        ARM_OP_PATCH<1, 2, 0, 1, true, 2>
#define ARM_OP_QSUB        ARM_OP_PATCH<1, 2, 0, 1, true, 2>
#define ARM_OP_QDADD       ARM_OP_PATCH<1, 2, 0, 1, true, 2>
#define ARM_OP_QDSUB       ARM_OP_PATCH<1, 2, 0, 1, true, 2>

#define ARM_OP_CLZ         ARM_OP_PATCH<0, 2, 0, 1, false, 2>

////////
// Need versions of these functions with exported symbol
u8  _MMU_read08_9(u32 addr) { return _MMU_read08(0, MMU_AT_DATA, addr); }
u8  _MMU_read08_7(u32 addr) { return _MMU_read08(1, MMU_AT_DATA, addr); }
u16 _MMU_read16_9(u32 addr) { return _MMU_read16(0, MMU_AT_DATA, addr); }
u16 _MMU_read16_7(u32 addr) { return _MMU_read16(1, MMU_AT_DATA, addr); }
u32 _MMU_read32_9(u32 addr) { return _MMU_read32(0, MMU_AT_DATA, addr); }
u32 _MMU_read32_7(u32 addr) { return _MMU_read32(1, MMU_AT_DATA, addr); }

void _MMU_write08_9(u32 addr, u8  val) { _MMU_write08(0, MMU_AT_DATA, addr, val); }
void _MMU_write08_7(u32 addr, u8  val) { _MMU_write08(1, MMU_AT_DATA, addr, val); }
void _MMU_write16_9(u32 addr, u16 val) { _MMU_write16(0, MMU_AT_DATA, addr, val); }
void _MMU_write16_7(u32 addr, u16 val) { _MMU_write16(1, MMU_AT_DATA, addr, val); }
void _MMU_write32_9(u32 addr, u32 val) { _MMU_write32(0, MMU_AT_DATA, addr, val); }
void _MMU_write32_7(u32 addr, u32 val) { _MMU_write32(1, MMU_AT_DATA, addr, val); }

static const uint32_t mem_funcs[12] =
{
   (uint32_t)_MMU_read08_9 , (uint32_t)_MMU_read08_7,
   (uint32_t)_MMU_write08_9, (uint32_t)_MMU_write08_7,
   (uint32_t)_MMU_read32_9,  (uint32_t)_MMU_read32_7,
   (uint32_t)_MMU_write32_9, (uint32_t)_MMU_write32_7,
   (uint32_t)_MMU_read16_9,  (uint32_t)_MMU_read16_7,
   (uint32_t)_MMU_write16_9, (uint32_t)_MMU_write16_7
};

static void ARM_OP_MEM_DO_INDEX(uint32_t opcode, reg_t nrn, reg_t nrm)
{
   // Uses nrm as scratch if loading a constant

   if (bit(opcode, 25))
   {
      const AG_ALU_SHIFT st = (AG_ALU_SHIFT)bit(opcode, 5, 2);
      const uint32_t imm = bit(opcode, 7, 5);

      if (bit(opcode, 23)) block->add(nrn, alu2::reg_shift_imm(nrm, st, imm));
      else                 block->sub(nrn, alu2::reg_shift_imm(nrm, st, imm));
   }
   else
   {
      block->load_constant(nrm, opcode & 0xFFF);

      if (bit(opcode, 23)) block->add(nrn, alu2::reg(nrm));
      else                 block->sub(nrn, alu2::reg(nrm));
   }
}

static OP_RESULT ARM_OP_MEM(uint32_t pc, uint32_t opcode)
{
   const bool has_reg_offset = bit(opcode, 25);
   const bool has_pre_index = bit(opcode, 24);
   const bool has_up_bit = bit(opcode, 23);
   const bool has_byte_bit = bit(opcode, 22);
   const bool has_write_back = bit(opcode, 21);
   const bool has_load = bit(opcode, 20);
   const reg_t rn = bit(opcode, 16, 4);
   const reg_t rd = bit(opcode, 12, 4);
   const reg_t rm = bit(opcode, 0, 4);

   if (rn == 0xF || rd == 0xF || (has_reg_offset && (rm == 0xF)))
      return OPR_INTERPRET;

   const reg_t dest = regman->get(rd);
   const reg_t base = regman->get(rn);
   const reg_t offs = has_reg_offset ? regman->get(rm) : (reg_t)3;

   load_status();
   block->b("run", (AG_COND)(opcode >> 28));
   block->b("skip");
   block->set_label("run");

   // Put the EA in R0
   block->mov(0, alu2::reg(base));

   if (has_pre_index)
   {
      ARM_OP_MEM_DO_INDEX(opcode, 0, offs);
   }

   if (has_write_back)
   {
      if (!has_pre_index)
      {
         ARM_OP_MEM_DO_INDEX(opcode, base, offs);
      }
      else
      {
         block->mov(base, alu2::reg(0));
      }

      regman->mark_dirty(base);
   }

   // DO
   if (!has_load)
   {
      block->mov(1, alu2::reg(dest));
   }

   uint32_t func_idx = block_procnum | (has_load ? 0 : 2) | (has_byte_bit ? 0 : 4);
   block->load_constant(2, mem_funcs[func_idx]);
   block->blx(2);

   if (has_load)
   {
      block->mov(dest, alu2::reg(0));
      regman->mark_dirty(dest);
   }

   block->set_label("skip");
   block->resolve_label("run");
   block->resolve_label("skip");

   // TODO: 
   block->add(RCYC, alu2::imm(3));

   return OPR_CONTINUE;
}

#define ARM_MEM_OP_DEF(T, Q) \
   static const ArmOpCompiler ARM_OP_##T##_LSL_##Q = ARM_OP_MEM; \
   static const ArmOpCompiler ARM_OP_##T##_LSR_##Q = ARM_OP_MEM; \
   static const ArmOpCompiler ARM_OP_##T##_ASR_##Q = ARM_OP_MEM; \
   static const ArmOpCompiler ARM_OP_##T##_ROR_##Q = ARM_OP_MEM

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

#define ARM_OP_STRH_POS_INDE_M_REG_OFF 0
#define ARM_OP_LDRD_STRD_POST_INDEX 0
#define ARM_OP_LDRH_POS_INDE_M_REG_OFF 0
#define ARM_OP_LDRSB_POS_INDE_M_REG_OFF 0
#define ARM_OP_LDRSH_POS_INDE_M_REG_OFF 0
#define ARM_OP_UND 0
#define ARM_OP_STRH_POS_INDE_M_IMM_OFF 0
#define ARM_OP_LDRH_POS_INDE_M_IMM_OFF 0
#define ARM_OP_LDRSB_POS_INDE_M_IMM_OFF 0
#define ARM_OP_LDRSH_POS_INDE_M_IMM_OFF 0
#define ARM_OP_STRH_POS_INDE_P_REG_OFF 0
#define ARM_OP_LDRH_POS_INDE_P_REG_OFF 0
#define ARM_OP_LDRSB_POS_INDE_P_REG_OFF 0
#define ARM_OP_LDRSH_POS_INDE_P_REG_OFF 0
#define ARM_OP_STRH_POS_INDE_P_IMM_OFF 0
#define ARM_OP_LDRH_POS_INDE_P_IMM_OFF 0
#define ARM_OP_LDRSB_POS_INDE_P_IMM_OFF 0
#define ARM_OP_LDRSH_POS_INDE_P_IMM_OFF 0
#define ARM_OP_MRS_CPSR 0
#define ARM_OP_SWP 0
#define ARM_OP_STRH_M_REG_OFF 0

#define ARM_OP_LDRD_STRD_OFFSET_PRE_INDEX 0
#define ARM_OP_LDRH_M_REG_OFF 0
#define ARM_OP_LDRSB_M_REG_OFF 0
#define ARM_OP_LDRSH_M_REG_OFF 0
#define ARM_OP_MSR_CPSR 0
#define ARM_OP_BX 0
#define ARM_OP_BLX_REG 0

#define ARM_OP_BKPT 0
#define ARM_OP_STRH_PRE_INDE_M_REG_OFF 0
#define ARM_OP_LDRH_PRE_INDE_M_REG_OFF 0
#define ARM_OP_LDRSB_PRE_INDE_M_REG_OFF 0
#define ARM_OP_LDRSH_PRE_INDE_M_REG_OFF 0
#define ARM_OP_MRS_SPSR 0

#define ARM_OP_SWPB 0
#define ARM_OP_STRH_M_IMM_OFF 0
#define ARM_OP_LDRH_M_IMM_OFF 0
#define ARM_OP_LDRSB_M_IMM_OFF 0
#define ARM_OP_LDRSH_M_IMM_OFF 0
#define ARM_OP_MSR_SPSR 0
#define ARM_OP_STRH_PRE_INDE_M_IMM_OFF 0
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

////////
// THUMB
////////
static OP_RESULT THUMB_OP_SHIFT(uint32_t pc, uint32_t opcode)
{
   const uint32_t rd = bit(opcode, 0, 3);
   const uint32_t rs = bit(opcode, 3, 3);
   const uint32_t imm = bit(opcode, 6, 5);
   const AG_ALU_SHIFT op = (AG_ALU_SHIFT)bit(opcode, 11, 2);

   const reg_t nrd = regman->get(rd);
   const reg_t nrs = regman->get(rs);

   load_status();
   block->movs(nrd, alu2::reg_shift_imm(nrs, op, imm));
   write_status();

   regman->mark_dirty(nrd);

   block->add(RCYC, alu2::imm(1));
   return OPR_CONTINUE;
}

static OP_RESULT THUMB_OP_ADDSUB_REGIMM(uint32_t pc, uint32_t opcode)
{
   const uint32_t rd = bit(opcode, 0, 3);
   const uint32_t rs = bit(opcode, 3, 3);
   const AG_ALU_OP op = bit(opcode, 9) ? SUBS : ADDS;
   const bool arg_type = bit(opcode, 10);
   const uint32_t arg = bit(opcode, 6, 3);

   const reg_t nrd = regman->get(rd);
   const reg_t nrs = regman->get(rs);

   if (arg_type) // Immediate
   {
      load_status();
      block->alu_op(op, nrd, nrs, alu2::imm(arg));
      write_status();
   }
   else
   {
      const reg_t narg = regman->get(arg);
      load_status();
      block->alu_op(op, nrd, nrs, alu2::reg(narg));
      write_status();
   }

   regman->mark_dirty(nrd);

   block->add(RCYC, alu2::imm(1));

   return OPR_CONTINUE;
}

static OP_RESULT THUMB_OP_MCAS_IMM8(uint32_t pc, uint32_t opcode)
{
   const reg_t rd = bit(opcode, 8, 3);
   const uint32_t op = bit(opcode, 11, 2);
   const uint32_t imm = bit(opcode, 0, 8);

   const reg_t nrd = regman->get(rd);

   load_status();
   
   switch (op)
   {
      case 0: block->alu_op(MOVS, nrd, nrd, alu2::imm(imm)); break;
      case 1: block->alu_op(CMP , nrd, nrd, alu2::imm(imm)); break;
      case 2: block->alu_op(ADDS, nrd, nrd, alu2::imm(imm)); break;
      case 3: block->alu_op(SUBS, nrd, nrd, alu2::imm(imm)); break;
   }

   write_status();

   if (op != 1) // Don't keep the result of a CMP instruction
   {
      regman->mark_dirty(nrd);
   }

   block->add(RCYC, alu2::imm(1));
   return OPR_CONTINUE;
}

static OP_RESULT THUMB_OP_ALU(uint32_t pc, uint32_t opcode)
{
   const uint32_t rd = bit(opcode, 0, 3);
   const uint32_t rs = bit(opcode, 3, 3);
   const uint32_t op = bit(opcode, 6, 4);
   bool need_writeback = false;

   if (op == 13) // TODO: The MULS is interpreted for now
   {
      return OPR_INTERPRET;
   }

   const reg_t nrd = regman->get(rd);
   const reg_t nrs = regman->get(rs);

   load_status();

   switch (op)
   {
      case  0: block->ands(nrd, alu2::reg(nrs)); break;
      case  1: block->eors(nrd, alu2::reg(nrs)); break;
      case  5: block->adcs(nrd, alu2::reg(nrs)); break;
      case  6: block->sbcs(nrd, alu2::reg(nrs)); break;
      case  8: block->tst (nrd, alu2::reg(nrs)); break;
      case 10: block->cmp (nrd, alu2::reg(nrs)); break;
      case 11: block->cmn (nrd, alu2::reg(nrs)); break;
      case 12: block->orrs(nrd, alu2::reg(nrs)); break;
      case 14: block->bics(nrd, alu2::reg(nrs)); break;
      case 15: block->mvns(nrd, alu2::reg(nrs)); break;

      case  2: block->movs(nrd, alu2::reg_shift_reg(nrd, LSL, nrs)); break;
      case  3: block->movs(nrd, alu2::reg_shift_reg(nrd, LSR, nrs)); break;
      case  4: block->movs(nrd, alu2::reg_shift_reg(nrd, ASR, nrs)); break;
      case  7: block->movs(nrd, alu2::reg_shift_reg(nrd, arm_gen::ROR, nrs)); break;

      case  9: block->rsbs(nrd, nrs, alu2::imm(0)); break;
   }

   write_status();

   static const bool op_wb[16] = { 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1 };
   if (op_wb[op])
   {
      regman->mark_dirty(nrd);
   }

   block->add(RCYC, alu2::imm(1));
   return OPR_CONTINUE;
}

static OP_RESULT THUMB_OP_SPE(uint32_t pc, uint32_t opcode)
{
   const uint32_t rd = bit(opcode, 0, 3) + (bit(opcode, 7) ? 8 : 0);
   const uint32_t rs = bit(opcode, 3, 4);
   const uint32_t op = bit(opcode, 8, 2);

   if (rd == 0xF || rs == 0xF)
   {
      return OPR_INTERPRET;
   }

   const reg_t nrd = regman->get(rd);
   const reg_t nrs = regman->get(rs);

   load_status();

   switch (op)
   {
      case 0: block->add(nrd, alu2::reg(nrs)); break;
      case 1: block->cmp(nrd, alu2::reg(nrs)); break;
      case 2: block->mov(nrd, alu2::reg(nrs)); break;
   }

   write_status();

   if (op != 1)
   {
      regman->mark_dirty(nrd);
   }

   block->add(RCYC, alu2::imm(1));

   return OPR_CONTINUE;
}

static OP_RESULT THUMB_OP_LDRSTR_REG_OFF(uint32_t pc, uint32_t opcode)
{
   const uint32_t rd = bit(opcode, 0, 3);
   const uint32_t rb = bit(opcode, 3, 3);
   const uint32_t ro = bit(opcode, 6, 3);
   const bool has_byte = bit(opcode, 10);
   const bool has_load = bit(opcode, 11);

   const reg_t dest = regman->get(rd);
   const reg_t base = regman->get(rb);
   const reg_t offs = regman->get(ro);

   block->mov(0, alu2::reg(base));
   block->add(0, alu2::reg(offs));

   if (!has_load)
   {
      block->mov(1, alu2::reg(dest));
   }

   uint32_t func_idx = block_procnum | (has_load ? 0 : 2) | (has_byte ? 0 : 4);
   block->load_constant(2, mem_funcs[func_idx]);
   block->blx(2);

   if (has_load)
   {
      block->mov(dest, alu2::reg(0));
      regman->mark_dirty(dest);
   }

   // TODO:
   block->add(RCYC, alu2::imm(3));
   return OPR_CONTINUE;
}


static OP_RESULT THUMB_OP_LDRSTR_IMM_OFF(uint32_t pc, uint32_t opcode)
{
   const uint32_t rd = bit(opcode, 0, 3);
   const uint32_t rb = bit(opcode, 3, 3);
   const uint32_t off = bit(opcode, 6, 5);
   const uint32_t op = bit(opcode, 13, 3);
   const bool has_load = bit(opcode, 11);
   const bool has_word = (op == 3) && !bit(opcode, 12);
   const bool has_half = (op == 4);

   const reg_t dest = regman->get(rd);
   const reg_t base = regman->get(rb);

   block->mov(0, alu2::reg(base));
   block->add(0, alu2::imm(off << ((has_word ? 2 : 0) + (has_half ? 1 : 0))));

   if (!has_load)
   {
      block->mov(1, alu2::reg(dest));
   }

   uint32_t func_idx = block_procnum | (has_load ? 0 : 2);
   func_idx |= has_word ? 4 : 0;
   func_idx |= has_half ? 8 : 0;
   block->load_constant(2, mem_funcs[func_idx]);
   block->blx(2);

   if (has_load)
   {
      block->mov(dest, alu2::reg(0));
      regman->mark_dirty(dest);
   }

   // TODO: 
   block->add(RCYC, alu2::imm(3));

   return OPR_CONTINUE;
}

static OP_RESULT THUMB_OP_LDRSTR_SIGN(uint32_t pc, uint32_t opcode)
{
   const uint32_t rd = bit(opcode, 0, 3);
   const uint32_t rb = bit(opcode, 3, 3);
   const uint32_t ro = bit(opcode, 6, 3);
   const uint32_t op = bit(opcode, 10, 2);

   const reg_t dest = regman->get(rd);
   const reg_t base = regman->get(rb);
   const reg_t offs = regman->get(ro);

   block->mov(0, alu2::reg(base));
   block->add(0, alu2::reg(offs));

   switch (op)
   {
      case 0: // STRH
         block->mov(1, alu2::reg(dest));
         block->load_constant(2, (uint32_t)mem_funcs[10 + block_procnum]);
         block->blx(2);
         break;

      case 1: // LDSB
         block->load_constant(2, (uint32_t)mem_funcs[0 + block_procnum]);
         block->blx(2);
         block->sxtb(dest, 0);
         regman->mark_dirty(dest);
         break;

      case 2: // LDRH
         block->load_constant(2, (uint32_t)mem_funcs[8 + block_procnum]);
         block->blx(2);
         block->uxth(dest, 0);
         regman->mark_dirty(dest);
         break;

      case 3: // LDSH
         block->load_constant(2, (uint32_t)mem_funcs[8 + block_procnum]);
         block->blx(2);
         block->sxth(dest, 0);
         regman->mark_dirty(dest);
         break;
   }

   // TODO:
   block->add(RCYC, alu2::imm(3));
   return OPR_CONTINUE;
}

static OP_RESULT THUMB_OP_B_COND(uint32_t pc, uint32_t opcode)
{
   const AG_COND cond = (AG_COND)bit(opcode, 8, 4);

   load_status();
   block->load_constant(0, pc + 2);
   block->load_constant(0, (pc + 4) + ((u32)((s8)(opcode&0xFF))<<1), cond);
   block->str(0, RCPU, mem2::imm(offsetof(armcpu_t, instruct_adr)));

   block->add(RCYC, alu2::imm(2), cond);
   block->add(RCYC, alu2::imm(1));

   return OPR_BRANCHED;
}

static OP_RESULT THUMB_OP_B_UNCOND(uint32_t pc, uint32_t opcode)
{
   int32_t offs = (opcode & 0x7FF) | (bit(opcode, 10) ? 0xFFFFF800 : 0);
   block->load_constant(0, pc + 4 + (offs << 1));

   block->str(0, RCPU, mem2::imm(offsetof(armcpu_t, instruct_adr)));

   block->add(RCYC, alu2::imm(3));

   return OPR_BRANCHED;
}

static OP_RESULT THUMB_OP_ADJUST_SP(uint32_t pc, uint32_t opcode)
{
   const uint32_t offs = bit(opcode, 0, 7);

   const reg_t sp = regman->get(13);

   if (bit(opcode, 7)) block->sub(sp, alu2::imm_rol(offs, 2));
   else                block->add(sp, alu2::imm_rol(offs, 2));

   regman->mark_dirty(sp);

   block->add(RCYC, alu2::imm(1));

   return OPR_CONTINUE;
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

#define THUMB_OP_ADD_SPE         THUMB_OP_SPE
#define THUMB_OP_CMP_SPE         THUMB_OP_SPE
#define THUMB_OP_MOV_SPE         THUMB_OP_SPE

#define THUMB_OP_STR_REG_OFF     THUMB_OP_LDRSTR_REG_OFF
#define THUMB_OP_STRB_REG_OFF    THUMB_OP_LDRSTR_REG_OFF
#define THUMB_OP_LDR_REG_OFF     THUMB_OP_LDRSTR_REG_OFF
#define THUMB_OP_LDRB_REG_OFF    THUMB_OP_LDRSTR_REG_OFF

#define THUMB_OP_STR_IMM_OFF     THUMB_OP_LDRSTR_IMM_OFF
#define THUMB_OP_LDR_IMM_OFF     THUMB_OP_LDRSTR_IMM_OFF
#define THUMB_OP_STRB_IMM_OFF    THUMB_OP_LDRSTR_IMM_OFF
#define THUMB_OP_LDRB_IMM_OFF    THUMB_OP_LDRSTR_IMM_OFF

#define THUMB_OP_STRH_IMM_OFF    THUMB_OP_LDRSTR_IMM_OFF
#define THUMB_OP_LDRH_IMM_OFF    THUMB_OP_LDRSTR_IMM_OFF

#define THUMB_OP_STRH_REG_OFF    THUMB_OP_LDRSTR_SIGN
#define THUMB_OP_LDRH_REG_OFF    THUMB_OP_LDRSTR_SIGN
#define THUMB_OP_LDRSH_REG_OFF   THUMB_OP_LDRSTR_SIGN
#define THUMB_OP_LDRSB_REG_OFF   THUMB_OP_LDRSTR_SIGN

#define THUMB_OP_ADJUST_P_SP     THUMB_OP_ADJUST_SP
#define THUMB_OP_ADJUST_M_SP     THUMB_OP_ADJUST_SP


// UNDEFINED OPS
#define THUMB_OP_BX_THUMB        THUMB_OP_INTERPRET
#define THUMB_OP_BLX_THUMB       THUMB_OP_INTERPRET
#define THUMB_OP_LDR_PCREL       THUMB_OP_INTERPRET
#define THUMB_OP_STR_SPREL       THUMB_OP_INTERPRET
#define THUMB_OP_LDR_SPREL       THUMB_OP_INTERPRET
#define THUMB_OP_ADD_2PC         THUMB_OP_INTERPRET
#define THUMB_OP_ADD_2SP         THUMB_OP_INTERPRET
#define THUMB_OP_PUSH            THUMB_OP_INTERPRET
#define THUMB_OP_PUSH_LR         THUMB_OP_INTERPRET
#define THUMB_OP_POP             THUMB_OP_INTERPRET
#define THUMB_OP_POP_PC          THUMB_OP_INTERPRET
#define THUMB_OP_BKPT_THUMB      THUMB_OP_INTERPRET
#define THUMB_OP_STMIA_THUMB     THUMB_OP_INTERPRET
#define THUMB_OP_LDMIA_THUMB     THUMB_OP_INTERPRET
#define THUMB_OP_SWI_THUMB       THUMB_OP_INTERPRET
#define THUMB_OP_BLX             THUMB_OP_INTERPRET
#define THUMB_OP_BL_10           THUMB_OP_INTERPRET
#define THUMB_OP_BL_11           THUMB_OP_INTERPRET

static const ArmOpCompiler thumb_instruction_compilers[1024] = {
#define TABDECL(x) THUMB_##x
#include "thumb_tabdef.inc"
#undef TABDECL
};



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
static ArmOpCompiled compile_basicblock()
{
   block_procnum = PROCNUM;

   const bool thumb = ARMPROC.CPSR.bits.T == 1;
   const u32 base = ARMPROC.instruct_adr;
   const u32 isize = thumb ? 2 : 4;

   uint32_t pc = base;
   bool compiled_op = true;
   bool has_ended = false;

   // NOTE: Expected register usage
   // R5 = Pointer to ARMPROC
   // R6 = Cycle counter

   regman->reset();
   block->push(0x4DF0);

   block->load_constant(RCPU, (uint32_t)&ARMPROC);
   block->load_constant(RCYC, 0);

   for (uint32_t i = 0; i < CommonSettings.jit_max_block_size && !has_ended; i ++, pc += isize)
   {
      uint32_t opcode = thumb ? _MMU_read16<PROCNUM, MMU_AT_CODE>(pc) : _MMU_read32<PROCNUM, MMU_AT_CODE>(pc);

      ArmOpCompiler compiler = thumb ? thumb_instruction_compilers[opcode >> 6]
                                     : arm_instruction_compilers[INSTRUCTION_INDEX(opcode)];

      OP_RESULT action = compiler ? compiler(pc, opcode) : OPR_INTERPRET;
      switch (action)
      {
         case OPR_INTERPRET:
         {
            if (compiled_op)
            {
               arm_jit_prefetch<PROCNUM>(pc, opcode, thumb);
               compiled_op = false;
            }

            regman->flush_all();
            regman->reset();

            block->load_constant(0, (uint32_t)&armcpu_exec<PROCNUM>);
            block->blx(0);
            block->add(RCYC, alu2::reg(0));

            has_ended = has_ended || instr_is_branch(thumb, opcode);

            break;
         }

         case OPR_BRANCHED:
         {
            has_ended = true;
            compiled_op = false;
            break;
         }

         case OPR_CONTINUE:
         {
            compiled_op = true;
            break;
         }
      }
   }

   if (compiled_op)
   {
      arm_jit_prefetch<PROCNUM>(pc, 0, thumb);
   }

   regman->flush_all();
   regman->reset();

   block->mov(0, alu2::reg(RCYC));
   block->pop(0x8DF0);

   void* fn_ptr = block->fn_pointer();
   JIT_COMPILED_FUNC(base, PROCNUM) = (uintptr_t)fn_ptr;
   return (ArmOpCompiled)fn_ptr;
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

   if (block->instructions_remaining() < 1000)
   {
      arm_jit_reset(true);
   }

   return compile_basicblock<PROCNUM>()();
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

      delete block;
      block = new code_pool(INSTRUCTION_COUNT);

      delete regman;
      regman = new register_manager(block);
   }
}

void arm_jit_close()
{
   delete block;
   block = 0;

   delete regman;
   regman = 0;
}
#endif // HAVE_JIT
