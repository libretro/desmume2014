/*
   Copyright (C) 2006 yopyop
   Copyright (C) 2006-2007 Theo Berkau
   Copyright (C) 2007 shash
   Copyright (C) 2009-2012 DeSmuME team

   This file is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 2 of the License, or
   (at your option) any later version.

   This file is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with the this software.  If not, see <http://www.gnu.org/licenses/>.
*/

union tile_entry_t
{
   tile_entry_t(u32 value_) : value(value_) { }

   u32 value;

   operator u32() const { return value; }

   struct
   {
      unsigned TileNum : 10;
      unsigned HFlip   : 1;
      unsigned VFlip   : 1;
      unsigned Palette : 4;
   } __attribute__((packed));
};

union rotocoord_t
{
   rotocoord_t(s32 value_) : val(value_) { }

   s32 val;

   struct
   {
      unsigned Fraction : 8;
      signed   Integer  : 20;
      unsigned pad      : 4;
   } __attribute__((packed));
};

const GPU::background::bg_type GPU::background::background_types[8][4] = 
{
   {Text,     Text,    Text,        Text       },
   {Text,     Text,    Text,        Affine     },
   {Text,     Text,    Affine,      Affine     },
   {Text,     Text,    Text,        AffineExt  },
   {Text,     Text,    Affine,      AffineExt  },
   {Text,     Text,    AffineExt,   AffineExt  },
   {Invalid,  Invalid, Large8bpp,   Invalid    },
   {Invalid,  Invalid, Invalid,     Invalid    }
};

static const u32 bg_size_table[8][4][2] =
{
   {{0,   0   }, {0,    0  }, {0,   0  }, {0,    0   }}, //Invalid
   {{256, 256 }, {512,  256}, {256, 512}, {512,  512 }}, //text
   {{128, 128 }, {256,  256}, {512, 512}, {1024, 1024}}, //affine
   {{512, 1024}, {1024, 512}, {0,   0  }, {0,    0   }}, //large 8bpp
   {{0,   0   }, {0,    0  }, {0,   0  }, {0,    0   }}, //affine ext (to be elaborated with another value)
   {{128, 128 }, {256,  256}, {512, 512}, {1024, 1024}}, //affine ext 256x16
   {{128, 128 }, {256,  256}, {512, 256}, {512,  512 }}, //affine ext 256x1
   {{128, 128 }, {256,  256}, {512, 256}, {512,  512 }}, //affine ext direct
};


/*****************************************************************************/
//         BACKGROUND RENDERING -TILE EXPANSION-
/*****************************************************************************/
struct TILE
{
   u32   tag;
   PIXEL pixels[8];
};

static TILE tile_cache[0x10000];
static u32  tile_tag;

static FORCEINLINE void expand_tile_4bit(PIXEL* const tile_buffer, const u8* const tile_pixels, const u16* const palette)
{
   for (int i = 0; i != 4; i ++)
   {
      const u8 pixels = tile_pixels[i];
      tile_buffer[i * 2 + 0] = BuildPIXEL(palette[pixels & 0xF], pixels & 0xF);
      tile_buffer[i * 2 + 1] = BuildPIXEL(palette[pixels >> 4], pixels >> 4);
   }
}

static FORCEINLINE void expand_tile_8bit(PIXEL* const tile_buffer, const u8* const tile_pixels, const u16* const palette)
{
   for (int i = 0; i != 8; i ++)
   {
      tile_buffer[i] = BuildPIXEL(palette[tile_pixels[i]], tile_pixels[i]);
   }
}

static FORCEINLINE void horizontal_flip_tile(PIXEL* const tile_buffer)
{
   PIXEL tmp[8];
   memcpy(tmp, tile_buffer, sizeof(tmp));

   for (int i = 0; i != 8; i ++)
   {
      tile_buffer[7 - i] = tmp[i];
   }
}

static FORCEINLINE const PIXEL* expand_tile(GPU* gpu, GPU::background& bg, tile_entry_t tile, u32 line, bool colors256, const u16* ex_palette)
{
   if (tile_cache[tile].tag != tile_tag)
   {
      PIXEL* tile_buffer = tile_cache[tile].pixels;
      tile_cache[tile].tag = tile_tag;

      line = ((tile.VFlip) ? 7 - line : line) & 7;

      const u32 tile_size = colors256 ? 64 : 32;
      const u32 line_size = colors256 ?  8 :  4;
      const u8* const tile_pixels = (u8*)MMU_gpu_map(bg.tile_pixel_ram + (tile.TileNum * tile_size) + (line * line_size));

      if (!colors256)
      {
         expand_tile_4bit(tile_buffer, tile_pixels, &gpu->palette[tile.Palette * 16]);
      }
      else
      {
         expand_tile_8bit(tile_buffer, tile_pixels, ex_palette ? &ex_palette[256 * tile.Palette] : gpu->palette);
      }

      if (tile.HFlip)
      {
         horizontal_flip_tile(tile_buffer);
      }
   }

   return tile_cache[tile].pixels;
}

/*****************************************************************************/
//         BACKGROUND RENDERING -TEXT-
/*****************************************************************************/
static void renderline_textBG(GPU* gpu, u32 start_x, u32 line, u32 width)
{
   // Change tile tag, effectively clearing the tile cache
   // TODO: Make tile cache more persistent
   tile_tag ++;

   GPU::background& bg       = gpu->get_current_background();
   const background_control_t bg_control = bg.get_control();

   // Get backgroud size info
   const u16 bg_width        = 32 + ((bg_control.ScreenSize & 1) ? 32 : 0);
   const u16 bg_height       = 32 + ((bg_control.ScreenSize & 2) ? 32 : 0);
   const u16 bg_width_mask   = (bg_width - 1);
   const u16 bg_height_mask  = (bg_height - 1);

   // Get the base Y location of the tile map
   const u32 y_tile          = (line / 8) & bg_height_mask;
   const u32 tile_map_base   = (bg.tile_map_ram + (y_tile & 31) * 64)
                             + ((y_tile >= 32) ? ADDRESS_STEP_512B << bg_control.ScreenSize : 0);

   const u32 x_offset        = start_x & 7;

   for (int i = 0; i < 33; i ++)
   {
      const u32 x_tile      = ((start_x / 8) + i) & bg_width_mask;
      const u32 tile_map    = tile_map_base + (x_tile & 0x1F) * 2
                            + ((x_tile >= 32) ? 32 * 32 * 2 : 0);

      const PIXEL* tile_line = expand_tile(gpu, bg, T1ReadWord(MMU_gpu_map(tile_map), 0), line & 7, bg_control.Palette_256, bg.get_extended_palette());

      for (int j = 0; j != 8; j ++)
      {
         bg.set_pixel<true>(i * 8 + j - (start_x & 7), tile_line[j]);
      }
   }
}

/*****************************************************************************/
//         BACKGROUND RENDERING -ROTOSCALE- PIXEL GETTERS
/*****************************************************************************/

//        bg : Background structure
//   x_pixel : X Coordinate of pixel to draw
//   y_pixel : Y Coordinate of pixel to draw
//   palette : Tile palette
typedef PIXEL (*mode2_get_pixel_fn)(GPU::background& bg, s32 x_pixel, s32 y_pixel, u8* palette);

FORCEINLINE PIXEL get_pixel_tiled_8bit_entry(GPU::background& bg, s32 x_pixel, s32 y_pixel, u8* palette)
{
   const u32 tileindex = *(u8*)MMU_gpu_map(bg.tile_map_ram + ((x_pixel / 8) + (y_pixel / 8) * (bg.width / 8)));
   const u32 x = (x_pixel & 7);
   const u32 y = (y_pixel & 7);

   const u32 palette_entry = *(u8*)MMU_gpu_map(bg.tile_pixel_ram + ((tileindex * 64) + (y * 8) + x));
   const u32 color = palette_entry ? T1ReadWord(palette, palette_entry << 1) : 0;

   return BuildPIXEL(color, palette_entry);
}

template<bool extPal> FORCEINLINE PIXEL get_pixel_tiled_16bit_entry(GPU::background& bg, s32 x_pixel, s32 y_pixel, u8* palette)
{
   void* const map_addr = MMU_gpu_map(bg.tile_map_ram + (((x_pixel / 8) + (y_pixel / 8) * (bg.width / 8)) * 2));
   
   tile_entry_t tileentry = T1ReadWord(map_addr, 0);

   const u16 x = ((tileentry.HFlip) ? 7 - (x_pixel) : (x_pixel)) & 7;
   const u16 y = ((tileentry.VFlip) ? 7 - (y_pixel) : (y_pixel)) & 7;

   const u8 palette_entry = *(u8*)MMU_gpu_map(bg.tile_pixel_ram + ((tileentry.TileNum * 64) + (y * 8) + x));
   const u16 color = palette_entry ? T1ReadWord(palette, (palette_entry + (extPal ? (tileentry.Palette * 256) : 0)) * 2) : 0;

   return BuildPIXEL(color, palette_entry);
}

FORCEINLINE PIXEL get_pixel_paletted_bitmap(GPU::background& bg, s32 x_pixel, s32 y_pixel, u8* palette)
{
   const u32 bitmap_ram = (bg.type == GPU::background::Large8bpp) ? bg.large_bitmap_ram : bg.bitmap_ram;
   const u32 palette_entry = *(u8*)MMU_gpu_map(bitmap_ram + (y_pixel * bg.width) + x_pixel);;
   const u32 color = palette_entry ? T1ReadWord(palette, palette_entry << 1) : 0;

   return BuildPIXEL(color, palette_entry);
}

FORCEINLINE PIXEL get_pixel_direct_bitmap(GPU::background& bg, s32 x_pixel, s32 y_pixel, u8* palette)
{
   const u32 color = T1ReadWord(MMU_gpu_map((bg.bitmap_ram) + ((x_pixel + y_pixel * bg.width) * 2)), 0);
   return BuildPIXEL(color, color & 0x8000);
}


/*****************************************************************************/
//         BACKGROUND RENDERING -ROTOSCALE- NO ROTO & NO SCALE
/*****************************************************************************/
// TODO: This urgently needs wrap and bounds support

template <typename TILE_TYPE>
static FORCEINLINE void mode2_tiled_no_rot_no_scale(GPU::background* bg, const u16* ex_palette = 0)
{
   // Change tile tag, effectively clearing the tile cache
   tile_tag ++;

   //
   affine_parameters_t& affine_params = bg->get_affine_parameters();
   const s32 x = rotocoord_t(affine_params.X).Integer;
   const s32 y = rotocoord_t(affine_params.Y).Integer;

   // Only draw if the line is visible
   if (y >= 0 && y < bg->height)
   {
      // Get backgroud size info
      const u16 bg_width        = bg->width / 8;
      const u16 bg_height       = bg->height / 8;

      // Get the base Y location of the tile map
      const u32 y_tile          = y / 8;
      const TILE_TYPE* tile_map = (const TILE_TYPE*)MMU_gpu_map(bg->tile_map_ram + y_tile * (bg_width * sizeof(TILE_TYPE)));

      for (int i = 0; i != 33; i ++)
      {
         const u32 tile_val = tile_map[x / 8 + i];
         const PIXEL* tile_line = expand_tile(bg->parent, *bg, tile_val, y & 7, true, ex_palette);

         for (int j = 0; j != 8; j ++)
         {
            bg->set_pixel<true>(i * 8 + j, tile_line[j]);
         }
      }
   }
}

static FORCEINLINE void mode2_paletted_bitmap_no_rot_no_scale(GPU::background* bg)
{
   affine_parameters_t& affine_params = bg->get_affine_parameters();
   const s32 x = rotocoord_t(affine_params.X).Integer;
   const s32 y = rotocoord_t(affine_params.Y).Integer;

   // Only draw if the line is visible
   if (y >= 0 && y < bg->height)
   {
      const u32 bitmap_ram = (bg->type == GPU::background::Large8bpp) ? bg->large_bitmap_ram : bg->bitmap_ram;
      const u8* const bitmap = (const u8* const)MMU_gpu_map(bitmap_ram + (y * bg->width) + x);

      for (int i = 0; i != 256; i ++)
      {
         const u32 color = bitmap[i] ? bg->parent->palette[bitmap[i]] : 0;
         bg->set_pixel<false>(i, BuildPIXEL(color, bitmap[i]));
      }
   }
}

static FORCEINLINE void mode2_direct_bitmap_no_rot_no_scale(GPU::background* bg)
{
   affine_parameters_t& affine_params = bg->get_affine_parameters();
   const s32 x = rotocoord_t(affine_params.X).Integer;
   const s32 y = rotocoord_t(affine_params.Y).Integer;

   // Only draw if the line is visible
   if (y >= 0 && y < bg->height)
   {
      const u16* const bitmap = (const u16* const)MMU_gpu_map(bg->bitmap_ram + ((y * bg->width) + x) * 2);

      for (int i = 0; i != 256; i ++)
      {
         bg->set_pixel<false>(i, BuildPIXEL(bitmap[i], bitmap[i] & 0x8000));
      }
   }
}

/*****************************************************************************/
//         BACKGROUND RENDERING -ROTOSCALE-
/*****************************************************************************/

template<mode2_get_pixel_fn get_pixel, bool WRAP>
FORCEINLINE void rot_scale_op(GPU::background& bg, const affine_parameters_t& params, u8 * pal)
{
   rotocoord_t x = params.X;
   rotocoord_t y = params.Y;

   const s32 dx = (s32)params.PA;
   const s32 dy = (s32)params.PC;

   // TODO: Don't check bounds every pixel, instead calculate how many pixels can be drawn or skipped before overflow or wrap   

   for(int i = 0; i < SCREEN_WIDTH; i ++, x.val += dx, y.val += dy)
   {
      s32 x_pos = x.Integer & ((WRAP) ? bg.width - 1 : 0xFFFFFFFF);
      s32 y_pos = y.Integer & ((WRAP) ? bg.height - 1 : 0xFFFFFFFF);

      if(WRAP || ((x_pos >= 0) && (x_pos < bg.width) && (y_pos >= 0) && (y_pos < bg.height)))
      {
         bg.set_pixel<false>(i, get_pixel(bg, x_pos, y_pos, pal));
      }
   }
}

template<mode2_get_pixel_fn fun>
FORCEINLINE void apply_rot_fun(GPU::background& bg, const affine_parameters_t& params, u8 * pal)
{
   if(bg.get_control().PaletteSet_Wrap)    rot_scale_op<fun,true> (bg, params, pal);   
   else                                    rot_scale_op<fun,false>(bg, params, pal);   
}

FORCEINLINE void rotBG2(GPU* gpu, const affine_parameters_t& params)
{
   GPU::background& bg = gpu->get_current_background();
   u8 *pal = MMU.ARM9_VMEM + gpu->core * 0x400;

   switch(bg.type)
   {
      case GPU::background::Affine:
      {
         apply_rot_fun<get_pixel_tiled_8bit_entry>(bg, params, pal);
         return;
      }

      case GPU::background::AffineExt_256x16:
      {
         if (gpu->get_display_control().ExBGxPalette_Enable)
         {
            pal = (u8*)bg.get_extended_palette();

            if (pal)
            {
               apply_rot_fun<get_pixel_tiled_16bit_entry<true> >(bg, params, pal);
            }
         }
         else
         {
            apply_rot_fun<get_pixel_tiled_16bit_entry<false> >(bg, params, pal);
         }

         return;
      }

      case GPU::background::AffineExt_256x1:
      case GPU::background::Large8bpp:
      {
         apply_rot_fun<get_pixel_paletted_bitmap>(bg, params, pal);
         return;
      }

      case GPU::background::AffineExt_Direct:
      {
         apply_rot_fun<get_pixel_direct_bitmap>(bg, params, NULL);
         return;
      }

      default:
         return;
   }
}

/**********************************/
// GPU::background
/**********************************/
//this handles writing in BGxCNT
void GPU::background::refresh_control()
{
   const u32 core = parent->core;
   const display_control_t display_control = parent->get_display_control();
   const background_control_t bg_control   = get_control();
   
   parent->resort_backgrounds();

   const u32 memory_base   = (core == GPU_SUB) ? MMU_BBG : MMU_ABG;
   tile_map_ram            = memory_base + ((core == GPU_MAIN) ? display_control.ScreenBase_Block * ADDRESS_STEP_64KB : 0);
   tile_pixel_ram          = memory_base + ((core == GPU_MAIN) ? display_control.CharacBase_Block * ADDRESS_STEP_64KB : 0);
   bitmap_ram              = memory_base;
   large_bitmap_ram        = memory_base;

   tile_map_ram           += bg_control.ScreenBase_Block * ADDRESS_STEP_2KB;
   tile_pixel_ram         += bg_control.CharacBase_Block * ADDRESS_STEP_16KB;
   bitmap_ram             += bg_control.ScreenBase_Block * ADDRESS_STEP_16KB;

   extended_palette_slot   = number + ((number < 2) ? bg_control.PaletteSet_Wrap * 2 : 0);

   type                    = background_types[display_control.BG_Mode][number];

   //clarify affine ext modes 
   if(type == AffineExt)
   {
      static const bg_type affine_modes[4] = { AffineExt_256x16, AffineExt_256x16, AffineExt_256x1, AffineExt_Direct };
      const u32 affine_mode = (bg_control.Palette_256 << 1) | (bg_control.CharacBase_Block & 1);

      type = affine_modes[affine_mode];
   }

   set_size(bg_size_table[type][bg_control.ScreenSize][0], bg_size_table[type][bg_control.ScreenSize][1]);
}

bool GPU::background::render_pixels(u32 line)
{
   const display_control_t display_control = parent->get_display_control();
   const bg_type base_type = background_types[display_control.BG_Mode][number];

   if (base_type == Text)
   {
        renderline_textBG(parent, get_x_offset(), line + get_y_offset(), 256);
   }
   else if ((base_type == Affine) || (base_type == AffineExt) || (base_type == Large8bpp))
   {
      affine_parameters_t& parms = get_affine_parameters();

      // No rotation, no scale
      if (parms.PA == 0x100 && parms.PC == 0)
      {
         switch(type)
         {
            case Affine:            mode2_tiled_no_rot_no_scale<u8> (this); break;
            case AffineExt_256x16:  mode2_tiled_no_rot_no_scale<u16>(this, get_extended_palette()); break;
            case AffineExt_Direct:  mode2_direct_bitmap_no_rot_no_scale(this); break;
            case AffineExt_256x1:   mode2_paletted_bitmap_no_rot_no_scale(this); break;
            case Large8bpp:         mode2_paletted_bitmap_no_rot_no_scale(this); break;
            default:                abort();
         }
      }
      // No rotation
/*    else if(parms.PC == 0)
      {
         switch(type)
         {
            case Affine:            mode2_tiled_no_rot<u8> (this); break;
            case AffineExt_256x16:  mode2_tiled_no_rot<u16>(this, get_extended_palette()); break;
            case AffineExt_Direct:  mode2_direct_bitmap_no_rot(this); break;
            case AffineExt_256x1:   mode2_paletted_bitmap_no_rot(this); break;
            case Large8bpp:         mode2_paletted_bitmap_no_rot(this); break;
            default:                abort();
         }
      }*/
      else
      {
         rotBG2(parent, parms);
      }

      parms.X += parms.PB;
      parms.Y += parms.PD;
   }
   else if (type == Invalid)
   {
      PROGINFO("Attempting to render an invalid BG type\n");
      return false;
   }

   return true;
}
