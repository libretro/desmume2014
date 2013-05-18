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


/*****************************************************************************/
//			BACKGROUND RENDERING -TILE EXPANSION-
/*****************************************************************************/
struct TILE
{
   u32   tag;
   PIXEL pixels[8];
};

static TILE tile_cache[0x10000];
static u32  tile_tag;

static FORCEINLINE void expand_tile_8(PIXEL* const tile_buffer, const u8* const tile_pixels, const u16* const palette)
{
   for (int i = 0; i != 4; i ++)
   {
      const u8 pixels = tile_pixels[i];
      tile_buffer[i * 2 + 0] = BuildPIXEL(palette[pixels & 0xF], pixels & 0xF);
      tile_buffer[i * 2 + 1] = BuildPIXEL(palette[pixels >> 4], pixels >> 4);
   }
}

static FORCEINLINE void expand_tile_16(PIXEL* const tile_buffer, const u8* const tile_pixels, const u16* const palette)
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
         expand_tile_8(tile_buffer, tile_pixels, &gpu->palette[tile.Palette * 16]);
      }
      else
      {
         expand_tile_16(tile_buffer, tile_pixels, ex_palette ? &ex_palette[256 * tile.Palette] : gpu->palette);
      }

      if (tile.HFlip)
      {
         horizontal_flip_tile(tile_buffer);
      }
   }

   return tile_cache[tile].pixels;
}

/*****************************************************************************/
//			BACKGROUND RENDERING -TEXT-
/*****************************************************************************/
static void renderline_textBG(GPU* gpu, PIXEL pixels[34 * 8], u32 start_x, u32 line, u32 width)
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

   // This is set so that pixels[8] will be the first displayed pixel
   PIXEL* const pixel_base   = &pixels[8 - (start_x & 7)];

   for (int i = 0; i < 33; i ++)
   {
      const u32 x_tile      = ((start_x / 8) + i) & bg_width_mask;
      const u32 tile_map    = tile_map_base + (x_tile & 0x1F) * 2
                            + ((x_tile >= 32) ? 32 * 32 * 2 : 0);

      const u16* ex_palette = (gpu->get_display_control().ExBGxPalette_Enable && bg_control.Palette_256) ? (u16*)MMU.ExtPal[gpu->core][bg.extended_palette_slot] : 0;

      const PIXEL* tile_line = expand_tile(gpu, bg, T1ReadWord(MMU_gpu_map(tile_map), 0), line & 7, bg_control.Palette_256, ex_palette);
      memcpy(&pixel_base[i * 8], tile_line, sizeof(PIXEL) * 8);
   }
}

/*****************************************************************************/
//			BACKGROUND RENDERING -ROTOSCALE-
/*****************************************************************************/

//        bg : Background structure
//   x_pixel : X Coordinate of pixel to draw
//   y_pixel : Y Coordinate of pixel to draw
//   palette : Tile palette

FORCEINLINE PIXEL rot_tiled_8bit_entry(GPU* gpu, GPU::background& bg, s32 x_pixel, s32 y_pixel, u8* palette)
{
	const u32 tileindex = *(u8*)MMU_gpu_map(bg.tile_map_ram + ((x_pixel / 8) + (y_pixel / 8) * (bg.width / 8)));
	const u32 x = (x_pixel & 7); 
	const u32 y = (y_pixel & 7);

	const u32 palette_entry = *(u8*)MMU_gpu_map(bg.tile_pixel_ram + ((tileindex<<6)+(y<<3)+x));
	const u32 color = T1ReadWord(palette, palette_entry << 1);

   return BuildPIXEL(color, palette_entry);
}

template<bool extPal> FORCEINLINE PIXEL rot_tiled_16bit_entry(GPU* gpu, GPU::background& bg, s32 x_pixel, s32 y_pixel, u8* palette)
{
	void* const map_addr = MMU_gpu_map(bg.tile_map_ram + (((x_pixel / 8) + (y_pixel / 8) * (bg.width / 8)) * 2));
	
	tile_entry_t tileentry = T1ReadWord(map_addr, 0);

	const u16 x = ((tileentry.HFlip) ? 7 - (x_pixel) : (x_pixel))&7;
	const u16 y = ((tileentry.VFlip) ? 7 - (y_pixel) : (y_pixel))&7;

	const u8 palette_entry = *(u8*)MMU_gpu_map(bg.tile_pixel_ram + ((tileentry.TileNum<<6)+(y<<3)+x));
	const u16 color = T1ReadWord(palette, (palette_entry + (extPal ? (tileentry.Palette<<8) : 0)) << 1);

   return BuildPIXEL(color, palette_entry);
}

FORCEINLINE PIXEL rot_256_map(GPU* gpu, GPU::background& bg, s32 x_pixel, s32 y_pixel, u8* palette)
{
   const u32 bitmap_ram = (bg.type == BGType_Large8bpp) ? bg.large_bitmap_ram : bg.bitmap_ram;

	u8* const adr = (u8*)MMU_gpu_map((bitmap_ram) + ((x_pixel + y_pixel * bg.width)));
	const u32 palette_entry = *adr;
	const u32 color = T1ReadWord(palette, palette_entry << 1);

   return BuildPIXEL(color, palette_entry);
}

FORCEINLINE PIXEL rot_BMP_map(GPU* gpu, GPU::background& bg, s32 x_pixel, s32 y_pixel, u8* palette)
{
	void* const adr = MMU_gpu_map((bg.bitmap_ram) + ((x_pixel + y_pixel * bg.width) * 2));
	const u32 color = T1ReadWord(adr, 0);
   return BuildPIXEL(color, color & 0x8000);
}

typedef PIXEL (*rot_fun)(GPU* gpu, GPU::background& bg, s32 x_pixel, s32 y_pixel, u8* palette);

template<rot_fun fun, bool WRAP>
FORCEINLINE void rot_scale_op(GPU* gpu, PIXEL pixels[34 * 8], GPU::background& bg, const affine_parameters_t& params, u8 * pal)
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
         pixels[8 + i] = fun(gpu, bg, x_pos, y_pos, pal);
      }
   }
}

template<rot_fun fun>
FORCEINLINE void apply_rot_fun(GPU* gpu, PIXEL pixels[34 * 8], GPU::background& bg, const affine_parameters_t& params, u8 * pal)
{
	if(bg.get_control().PaletteSet_Wrap)    rot_scale_op<fun,true> (gpu, pixels, bg, params, pal);	
	else                                    rot_scale_op<fun,false>(gpu, pixels, bg, params, pal);	
}

FORCEINLINE void rotBG2(GPU* gpu, PIXEL pixels[34 * 8], const affine_parameters_t& params)
{
   GPU::background& bg = gpu->get_current_background();
	u8 *pal = MMU.ARM9_VMEM + gpu->core * 0x400;

	switch(bg.type)
	{
      case BGType_Affine:
      {
         apply_rot_fun<rot_tiled_8bit_entry>(gpu, pixels, bg, params, pal);
         return;
      }

   	case BGType_AffineExt_256x16:
      {
         if (gpu->get_display_control().ExBGxPalette_Enable)
         {
            pal = MMU.ExtPal[gpu->core][bg.extended_palette_slot];

            if (pal)
            {
               apply_rot_fun<rot_tiled_16bit_entry<true> >(gpu, pixels, bg, params, pal);
            }
         }
         else
         {
            apply_rot_fun<rot_tiled_16bit_entry<false> >(gpu, pixels, bg, params, pal);
         }

         return;
      }

      case BGType_AffineExt_256x1:
   	case BGType_Large8bpp:
      {
   		apply_rot_fun<rot_256_map>(gpu, pixels, bg, params, pal);
         return;
      }

      case BGType_AffineExt_Direct:
      {
		   apply_rot_fun<rot_BMP_map>(gpu, pixels, bg, params, NULL);
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

   const u32 gpu_memory_base  = (core == GPU_SUB) ? MMU_BBG : MMU_ABG;
   tile_map_ram            = gpu_memory_base + ((core == GPU_MAIN) ? display_control.ScreenBase_Block * ADDRESS_STEP_64KB : 0);
   tile_pixel_ram          = gpu_memory_base + ((core == GPU_MAIN) ? display_control.CharacBase_Block * ADDRESS_STEP_64KB : 0);
   bitmap_ram              = gpu_memory_base;
   large_bitmap_ram        = gpu_memory_base;

	tile_map_ram           += bg_control.ScreenBase_Block * ADDRESS_STEP_2KB;
	tile_pixel_ram         += bg_control.CharacBase_Block * ADDRESS_STEP_16KB;
	bitmap_ram             += bg_control.ScreenBase_Block * ADDRESS_STEP_16KB;

   extended_palette_slot   = number + ((number < 2) ? bg_control.PaletteSet_Wrap * 2 : 0);

	type                    = GPU_mode2type[display_control.BG_Mode][number];

	//clarify affine ext modes 
	if(type == BGType_AffineExt)
	{
      static const BGType affine_modes[4] = { BGType_AffineExt_256x16, BGType_AffineExt_256x16, BGType_AffineExt_256x1, BGType_AffineExt_Direct };
		const u32 affine_mode = (bg_control.Palette_256 << 1) | (bg_control.CharacBase_Block & 1);

      type = affine_modes[affine_mode];
	}

   set_size(sizeTab[type][bg_control.ScreenSize][0], sizeTab[type][bg_control.ScreenSize][1]);
}

bool GPU::background::render_pixels(u32 line, PIXEL pixels[34 * 4])
{
   const BGType type = GPU_mode2type[parent->get_display_control().BG_Mode][number];

   if (type == BGType_Text)
   {
     	renderline_textBG(parent, pixels, get_x_offset(), line + get_y_offset(), 256);
   }
   else if ((type == BGType_Affine) || (type == BGType_AffineExt) || (type == BGType_Large8bpp))
   {
   	affine_parameters_t& parms = get_affine_parameters();
      rotBG2(parent, pixels, parms);

   	parms.X += parms.PB;
	   parms.Y += parms.PD;
	}
   else if (type == BGType_Invalid)
   {
      PROGINFO("Attempting to render an invalid BG type\n");
      return false;
   }

   return true;
}

