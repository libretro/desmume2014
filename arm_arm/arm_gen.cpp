#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

#include "arm_gen.h"

// __clear_cache(start, end)
#ifdef __BLACKBERRY_QNX__
#undef __clear_cache
#define __clear_cache(start,end) msync(start, (size_t)((void*)end - (void*)start), MS_SYNC | MS_CACHE_ONLY | MS_INVALIDATE_ICACHE);
#elif defined(__MACH__)
#include <libkern/OSCacheControl.h>
#define __clear_cache mach_clear_cache
static void __clear_cache(void *start, void *end) {
  size_t len = (char *)end - (char *)start;
  sys_dcache_flush(start, len);
  sys_icache_invalidate(start, len);
}
#endif

namespace arm_gen
{

static const uint32_t INSTRUCTION_COUNT = 0x400000;
static uint32_t _instructions[INSTRUCTION_COUNT] __attribute__((aligned(4096)));

static const uint32_t BLOCK_COUNT = 2048;
static iblock _blocks[BLOCK_COUNT];
static void reset_blocks();

bool init()
{
   if (mprotect(_instructions, sizeof(_instructions), PROT_READ | PROT_WRITE | PROT_EXEC) != 0)
   {
      fprintf(stderr, "mprotect() failed\n");
      return false;
   }

   reset_blocks();

   return true;
}

/////////////////////////////////
// Instruction Block List
static void reset_blocks()
{
   memset(_blocks, 0, sizeof(_blocks));
   for (int i = 0; i < BLOCK_COUNT; i ++)
      _blocks[i].first = 0xFFFFFFFF;
}

iblock* find_block_for(uint32_t pc, uint32_t tag)
{
   for (int i = 0; i < BLOCK_COUNT; i ++)
   {
      if (_blocks[i].pc == pc && _blocks[i].tag == tag)
      {
         return &_blocks[i];
      }
   }

   return 0;
}

iblock* get_empty_block(uint32_t pc, uint32_t tag)
{
   uint32_t last_block_end = 0;

   for (int i = 0; i < BLOCK_COUNT; i ++)
   {
      if (_blocks[i].first == 0xFFFFFFFF)
      {
         _blocks[i].first = last_block_end;
         _blocks[i].pc = pc;
         _blocks[i].tag = tag;

         if (_blocks[i].first > (INSTRUCTION_COUNT - 10000))
            break;

         return &_blocks[i];
      }
      else
         last_block_end = _blocks[i].first + _blocks[i].count;
   }

   reset_blocks();
   return 0;
}

/////////////////////////////////
// Instruction Blocks
void iblock::cache_flush()
{
   __clear_cache(&_instructions[first], &_instructions[first + count]);
}

void* iblock::fn_pointer() const
{
   return (void*)&_instructions[first];
}

void iblock::set_label(const char* name)
{
   for (int i = 0; i < TARGET_COUNT; i ++)
   {
      if (labels[i].name == 0)
      {
         labels[i].name = name;
         labels[i].position = count;
         return;
      }
   }

   assert(false);
}

void iblock::resolve_label(const char* name)
{
   for (int i = 0; i < TARGET_COUNT; i ++)
   {
      if (labels[i].name != name)
      {
         continue;
      }

      for (int j = 0; j < TARGET_COUNT; j ++)
      {
         if (branches[j].name != name)
         {
            continue;
         }

         const uint32_t source = branches[j].position;
         const uint32_t target = labels[i].position;
         _instructions[first + source] |= ((target - source) - 2) & 0xFFFFFF;

         branches[j].name = 0;
      }

      labels[i].name = 0;
      break;
   }
}

void iblock::insert_instruction(uint32_t op, AG_COND cond)
{
   assert(cond < CONDINVALID);
   _instructions[first + count ++] = (op & 0x0FFFFFFF) | (cond << 28);
}

void iblock::insert_raw_instruction(uint32_t op)
{
   _instructions[first + count ++] = op;
}

void iblock::alu_op(AG_ALU_OP op, reg_t rd, reg_t rn,
                     const alu2& arg, AG_COND cond)
{
   assert(op < OPINVALID);
   insert_instruction( (op << 20) | (rn << 16) | (rd << 12) | arg.encoding, cond );
}

void iblock::mem_op(AG_MEM_OP op, reg_t rd, reg_t rn, const mem2& arg,
                         AG_MEM_FLAGS flags, AG_COND cond)
{
   uint32_t instruction = 0x04000000;
   instruction |= (op & 1) ? 1 << 20 : 0;
   instruction |= (op & 2) ? 1 << 22 : 0;

   instruction |= arg.encoding;
   instruction |= rd << 12;
   instruction |= rn << 16;

   instruction |= flags ^ 0x1800000;

   insert_instruction( instruction, cond );
}

void iblock::b(const char* target, AG_COND cond)
{
   assert(target);

   for (int i = 0; i < TARGET_COUNT; i ++)
   {
      if (branches[i].name == 0)
      {
         branches[i].name = target;
         branches[i].position = count;
         insert_instruction( 0x0A000000, cond );
         return;
      }
   }

   assert(false);
}

void iblock::load_constant(reg_t target_reg, uint32_t constant, AG_COND cond)
{
   // TODO: Support another method for ARM procs that don't have movw|movt

   uint32_t instructions[2] = { 0x03000000, 0x03400000 };

   for (int i = 0; i < 2; i ++, constant >>= 16)
   {
      // If the upper 16-bits are zero the movt op is not needed
      if (i == 1 && constant == 0)
         break;

      instructions[i] |= target_reg << 12;
      instructions[i] |= constant & 0xFFF;
      instructions[i] |= (constant & 0xF000) << 4;
      insert_instruction( instructions[i], cond );
   }
}

// DEBUG ONLY
const uint32_t* iblock::get_instruction_stream() const
{
   return &_instructions[first];
}


} // namespace arm_gen