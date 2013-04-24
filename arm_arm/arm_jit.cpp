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

static u8 recompile_counts[(1<<26)/16];

static bool bb_thumb;
static const reg_t bb_cpu = 7;
static const reg_t bb_cycles = 6;


DS_ALIGN(4096) uintptr_t compiled_funcs[1<<26] = {0};

static bool bit(uint32_t value, uint32_t bit)
{
   return value & (1 << bit);
}

static uint32_t bit(uint32_t value, uint32_t first, uint32_t count)
{
   return (value >> first) & ((1 << count) - 1);
}


// sequencer.reschedule = true;
#define changeCPSR { \
			X86CompilerFuncCall* ctxCPSR = c.call((void*)NDS_Reschedule); \
			ctxCPSR->setPrototype(ASMJIT_CALL_CONV, FuncBuilder0<void>()); \
}


// ============================================================================================= IMM

//-----------------------------------------------------------------------------
//   Compiler
//-----------------------------------------------------------------------------

static u32 instr_attributes(u32 opcode)
{
	return bb_thumb ? thumb_attributes[opcode>>6]
		 : instruction_attributes[INSTRUCTION_INDEX(opcode)];
}

static bool instr_is_branch(u32 opcode)
{
	u32 x = instr_attributes(opcode);
	if(bb_thumb)
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

/*static void arm_load_status(iblock* block)
{
   block->ldr(0, 7, mem2::imm(offsetof(armcpu_t, CPSR)));
   block->set_status(0);
}

static void arm_stash_status(iblock* block)
{
   block->get_status(0);
   block->mov(0, alu2::shift_imm(0, LSR, 24));
   block->strb(0, 7, mem2::imm(offsetof(armcpu_t, CPSR) + 3));
}

static void arm_arm_prefetch(iblock* block)
{
   block->ldr(0, 7, mem2::imm(offsetof(armcpu_t, next_instruction)));
   block->load_constant(1, 0xFFFFFFFC);
   block->and_(0, alu2::reg(1));

   block->str(0, 7, mem2::imm(offsetof(armcpu_t, instruct_adr)));

   block->add(0, alu2::imm(4));
   block->str(0, 7, mem2::imm(offsetof(armcpu_t, next_instruction)));

   block->add(0, alu2::imm(4));
   block->str(0, 7, mem2::imm(offsetof(armcpu_t, R) + 15 * 4));
}

static void arm_thumb_prefetch(iblock* block)
{
   block->ldr(0, 7, mem2::imm(offsetof(armcpu_t, next_instruction)));
   block->load_constant(1, 0xFFFFFFFE);
   block->and_(0, alu2::reg(1));

   block->str(0, 7, mem2::imm(offsetof(armcpu_t, instruct_adr)));

   block->add(0, alu2::imm(2));
   block->str(0, 7, mem2::imm(offsetof(armcpu_t, next_instruction)));

   block->add(0, alu2::imm(2));
   block->str(0, 7, mem2::imm(offsetof(armcpu_t, R) + 15 * 4));
}


static void arm_call_interpreter(iblock* block, u32 pc, u32 opcode)
{
   // st
   arm_stash_status(block);

   block->load_constant(0, opcode);

   if (!bb_thumb)
   {
      block->load_constant(1, (uint32_t)arm_instructions_set[PROCNUM][INSTRUCTION_INDEX(opcode)]);
      block->blx(1, (AG_COND)bit(opcode, 28, 4));
      block->add(6, 6, alu2::reg(0));
      arm_arm_prefetch(block);
   }
   else
   {
      block->load_constant(1, (uint32_t)thumb_instructions_set[PROCNUM][opcode>>6]);
      block->blx(1);
      block->add(6, 6, alu2::reg(0));
      arm_thumb_prefetch(block);
   }

   arm_load_status(block);
}*/

template<int PROCNUM>
static ArmOpCompiled compile_basicblock(iblock* block)
{
   bool thumb = ARMPROC.CPSR.bits.T;
   u32 base = ARMPROC.instruct_adr;
   u32 isize = thumb ? 2 : 4;


   block->push(0x40F0);                          // push {r4-r7, r14}

   block->load_constant(7, (uint32_t)&ARMPROC);  // r7 = cpu_state
   block->load_constant(6, 0);                   // r6 = cycle count
   block->load_constant(5, (uint32_t)&armcpu_exec<PROCNUM>);

	for (uint32_t i = 0; i < CommonSettings.jit_max_block_size; i ++)
	{
      uint32_t pc = base + i * isize;
      uint32_t opcode = bb_thumb ? _MMU_read16<PROCNUM, MMU_AT_CODE>(pc) : _MMU_read32<PROCNUM, MMU_AT_CODE>(pc);

      block->blx(5);
      block->add(6, 6, alu2::reg(0));

      if (instr_is_branch(opcode))
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
//	if(((recompile_counts[mask_adr >> 1] >> 4*(mask_adr & 1)) & 0xF) > 8)
//	{
//		ArmOpCompiled f = op_decode[PROCNUM][cpu->CPSR.bits.T];
//		JIT_COMPILED_FUNC(adr, PROCNUM) = (uintptr_t)f;
//		return f();
//	}
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
