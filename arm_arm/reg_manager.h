#ifndef ARM_JIT_REG_MANAGER_H
#define ARM_JIT_REG_MANAGER_H

#include "arm_gen.h"
#include "armcpu.h"

extern const arm_gen::reg_t RCPU;

class register_manager
{
   public:
      register_manager(arm_gen::code_pool* apool) : pool(apool)
      {
         reset();
      }

      void reset()
      {
         memset(mapping, 0xFF, sizeof(mapping));
         memset(usage_tag, 0, sizeof(usage_tag));
         memset(dirty, 0, sizeof(dirty));
         next_usage_tag = 1;
      }

      bool is_usable(arm_gen::reg_t reg) const
      {
         static const uint32_t USE_MAP = 0xDC0;
         return (USE_MAP & (1 << reg)) ? true : false;
      }

   private:
      int32_t find(uint32_t emu_reg_id)
      {
         for (int i = 0; i != 16; i ++)
         {
            if (is_usable(i) && mapping[i] == emu_reg_id)
            {
               usage_tag[i] = next_usage_tag ++;
               assert(is_usable(i));
               return i;
            }
         }

         return -1;
      }

      arm_gen::reg_t get_oldest()
      {
         uint32_t result = 0;
         uint32_t lowtag = 0xFFFFFFFF;

         for (int i = 0; i != 16; i ++)
         {
            if (is_usable(i) && usage_tag[i] < lowtag)
            {
               lowtag = usage_tag[i];
               result = i;
            }
         }

         assert(is_usable(result));
         return result;
      }

   public:
      arm_gen::reg_t get(uint32_t emu_reg_id)
      {
         int32_t current = find(emu_reg_id);
         if (current >= 0)
         {
            assert(is_usable(current));
            return current;
         }

         arm_gen::reg_t result = get_oldest();
         flush(result);

         mapping[result] = emu_reg_id;
         usage_tag[result] = next_usage_tag ++;
         read_emu(result, emu_reg_id);

         return result;
      }

      void mark_dirty(uint32_t native_reg)
      {
         assert(is_usable(native_reg));
         dirty[native_reg] = true;
      }

      void flush(uint32_t native_reg)
      {
         assert(is_usable(native_reg));
         if (dirty[native_reg])
         {
            write_emu(native_reg, mapping[native_reg]);
            dirty[native_reg] = false;
         }
      }

      void flush_all()
      {
         for (int i = 0; i != 16; i ++)
         {
            if (is_usable(i))
            {
               flush(i);
            }
         }
      }

   private:
      void read_emu(arm_gen::reg_t native, arm_gen::reg_t emu)
      {
         pool->ldr(native, RCPU, arm_gen::mem2::imm(offsetof(armcpu_t, R) + 4 * emu));
      }

      void write_emu(arm_gen::reg_t native, arm_gen::reg_t emu)
      {
         pool->str(native, RCPU, arm_gen::mem2::imm(offsetof(armcpu_t, R) + 4 * emu));
      }

   private:
      arm_gen::code_pool* pool;

      uint32_t mapping[16]; // Mapping[native] = emu
      uint32_t usage_tag[16];
      bool dirty[16];

      uint32_t next_usage_tag;
};

#endif
