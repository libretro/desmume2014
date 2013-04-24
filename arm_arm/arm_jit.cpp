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

static void read_emu_register(iblock* block, reg_t native, reg_t emu, AG_COND cond = AL)
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

static void write_status(iblock* block)
{
   block->get_status(0);
   block->mov(0, alu2::reg_shift_imm(0, LSR, 24));
   block->strb(0, 7, mem2::imm(offsetof(armcpu_t, CPSR) + 3));
}

template <int PROCNUM>
static void thumb_prefetch(iblock* block)
{
   block->ldr(0, 7, mem2::imm(offsetof(armcpu_t, next_instruction)));
   block->load_constant(1, 0xFFFFFFFE);
   block->and_(0, alu2::reg(1));

   block->str(0, 7, mem2::imm(offsetof(armcpu_t, instruct_adr)));

   block->add(0, alu2::imm(2));
   block->str(0, 7, mem2::imm(offsetof(armcpu_t, next_instruction)));

   block->add(0, alu2::imm(2));
   write_emu_register(block, 0, 15);

   block->sub(0, 0, alu2::imm(4));
   block->load_constant(1, (uint32_t)&_MMU_read16<PROCNUM, MMU_AT_CODE>);
   block->blx(1);
   block->str(0, 7, mem2::imm(offsetof(armcpu_t, instruction)));
}


////////
// THUMB
////////
static const uint8_t thumb_op_types[] =
{
    1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, 
    1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, 
    1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, 
    1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, 
    1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, 
    1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, 
    2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2, 
    2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2, 
    3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3, 
    3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3, 
    3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3, 
    3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3, 
    3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3, 
    3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3, 
    3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3, 
    3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3, 
    4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4, 
    5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5, 
    6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6, 
    6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6, 
    7,  7,  7,  7,  7,  7,  7,  7,  8,  8,  8,  8,  8,  8,  8,  8, 
    7,  7,  7,  7,  7,  7,  7,  7,  8,  8,  8,  8,  8,  8,  8,  8, 
    7,  7,  7,  7,  7,  7,  7,  7,  8,  8,  8,  8,  8,  8,  8,  8, 
    7,  7,  7,  7,  7,  7,  7,  7,  8,  8,  8,  8,  8,  8,  8,  8, 
    9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9, 
    9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9, 
    9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9, 
    9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9, 
    9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9, 
    9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9, 
    9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9, 
    9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9, 
   10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 
   10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 
   10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 
   10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 
   11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 
   11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 
   11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 
   11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 
   12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 
   12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 
   12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 
   12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 
   13, 13, 13, 13,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
   14, 14, 14, 14, 14, 14, 14, 14,  0,  0,  0,  0,  0,  0,  0,  0, 
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
   14, 14, 14, 14, 14, 14, 14, 14,  0,  0,  0,  0,  0,  0,  0,  0, 
   15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 
   15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 
   15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 
   15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 
   16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 
   16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 
   16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 
   16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 
   17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 
   17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
   18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 
   18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 
   18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 
   18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18
};

template<int PROCNUM>
static bool arm_thumb_gen_code_for(iblock* block, uint32_t address, uint32_t opcode)
{
   // Extract common fields
   const uint32_t RD_ = bit(opcode, 0, 3);
   const uint32_t RS_ = bit(opcode, 3, 3);

   switch (thumb_op_types[opcode >> 6])
   {
      case 1: // move shifted register
      {
         const uint32_t imm = bit(opcode, 6, 5);
         const AG_ALU_SHIFT op = (AG_ALU_SHIFT)bit(opcode, 11, 2);

         load_status(block);

         read_emu_register(block, 0, RS_);
         block->movs(0, alu2::reg_shift_imm(0, op, imm));
         write_emu_register(block, 0, RD_);

         write_status(block);

         break;
      }

      case 2: // add/subtract
      {
         const AG_ALU_OP op = bit(opcode, 9) ? SUBS : ADDS;
         const bool arg_type = bit(opcode, 10);
         const uint32_t arg = bit(opcode, 6, 3);

         load_status(block);
         read_emu_register(block, 0, RS_);

         if (arg_type) // Immediate
         {
            block->alu_op(op, 0, 0, alu2::imm(arg));
         }
         else
         {
            read_emu_register(block, 1, arg);
            block->alu_op(op, 0, 0, alu2::reg(1));
         }

         write_emu_register(block, 0, RD_);
         write_status(block);
         break;
      }

      case 3: // move/compare/add/subtract immediate
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
         break;
      }

      case 4: // ALU operations
      {
         const uint32_t op = bit(opcode, 6, 4);
         bool need_writeback = false;

         if (op == 13) // TODO: The MULS is interpreted for now
         {
            return false;
         }

         load_status(block);
         read_emu_register(block, 0, RD_);
         read_emu_register(block, 1, RS_);

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
            write_emu_register(block, 0, RD_);
         }

         write_status(block);

         break;
      }

#if 0
      case 5: // Hi register operations/branch exchange
      case 6: // PC-relative load
      case 7: // load/store with register offset
      case 8: // load/store sign-extended byte/halfword
      case 9: // load/store with immediate offset
      case 10: // load/store halfword
      case 11: // SP-relative load/store
      case 12: // load address
      case 13: // add offset to Stack Pointer
      case 14: // push/pop registers
      case 15: // multiple load/store
      case 16: // conditional branch | software interrupt
      case 17: // unconditional branch
      case 18: // long branch with link
#endif
      default: return false;
   }

   // HACK
   block->add(6, alu2::imm(4)); // HACK!
 
   thumb_prefetch<PROCNUM>(block);
   return true;

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

/*
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
}*/

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

      if (!(thumb && arm_thumb_gen_code_for<PROCNUM>(block, pc, opcode)))
      {
         block->blx(5);
         block->add(6, 6, alu2::reg(0));
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
