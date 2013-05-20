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

struct size
{
	 s16 x;
	 s16 y;
};

static const size sprSizeTab[4][4] = 
{
     {{8,   8}, {16,  8}, {8,  16}, {8, 8}},
     {{16, 16}, {32,  8}, {8,  32}, {8, 8}},
     {{32, 32}, {32, 16}, {16, 32}, {8, 8}},
     {{64, 64}, {64, 32}, {32, 64}, {8, 8}},
};

union oam_object_t
{
   u16 attr[4];

   const size& get_size() const { return sprSizeTab[Size][Shape]; }

   struct
   {
      //attr0
      unsigned Y:8;
      unsigned RotScale:2;
      unsigned Mode:2;
      unsigned Mosaic:1;
      unsigned Depth:1;
      unsigned Shape:2;
      //att1
      signed X:9;
      unsigned RotScalIndex:3;
      unsigned HFlip:1;
      unsigned VFlip:1;
      unsigned Size:2;
      //attr2
      unsigned TileIndex:10;
      unsigned Priority:2;
      unsigned PaletteIndex:4;
      //attr3
   	unsigned attr3:16;
   };
};

//gets the affine parameter associated with the specified oam index.
u16 SlurpOAMAffineParam(const void* oam_buffer, int oam_index)
{
	u16* u16_oam_buffer = (u16*)oam_buffer;
	int u16_offset = oam_index<<2;
	return LE_TO_LOCAL_16(u16_oam_buffer[u16_offset + 3]);
}

/*****************************************************************************/
//			SPRITE RENDERING -HELPER FUNCTIONS-
/*****************************************************************************/

/* if i understand it correct, and it fixes some sprite problems in chameleon shot */
/* we have a 15 bit color, and should use the pal entry bits as alpha ?*/
/* http://nocash.emubase.de/gbatek.htm#dsvideoobjs */
INLINE void render_sprite_BMP (GPU::oam_t& oam, u16 l, u32 srcadr, u8 prio, int lg, int sprX, int x, int xdir, u8 alpha) 
{
	int i; u16 color;
	for(i = 0; i < lg; i++, ++sprX, x+=xdir)
	{
		u16* src = (u16*)MMU_gpu_map(srcadr+(x<<1));
		color = LE_TO_LOCAL_16(*src);

		// alpha bit = invisible
		if (color&0x8000)
		{
         oam.set_pixel<true>(sprX, color, true, alpha, prio, 3);
		}
	}
}

INLINE void render_sprite_256(GPU::oam_t& oam, u16 l, u32 srcadr, u16 * pal, u8 prio, int lg, int sprX, int x, int xdir, u8 alpha)
{
	int i; 
	u8 palette_entry; 
	u16 color;

	for(i = 0; i < lg; i++, ++sprX, x+=xdir)
	{
		u32 adr = srcadr + (x&0x7) + ((x&0xFFF8)<<3);
		u8* src = (u8 *)MMU_gpu_map(adr);
		palette_entry = *src;
		color = LE_TO_LOCAL_16(pal[palette_entry]);

		// palette entry = 0 means backdrop
		if (palette_entry>0)
		{
         oam.set_pixel<true>(sprX, color, true, 16, prio, alpha ? 1 : 0);
		}
	}
}

INLINE void render_sprite_16 (GPU::oam_t& oam, u16 l, u32 srcadr, u16 * pal, u8 prio, int lg, int sprX, int x, int xdir, u8 alpha)
{
	int i; 
	u8 palette, palette_entry;
	u16 color, x1;

	for(i = 0; i < lg; i++, ++sprX, x+=xdir)
	{
		x1 = x>>1;
		
		u32 adr = srcadr + (x1&0x3) + ((x1&0xFFFC)<<3);
		u8* src = (u8 *)MMU_gpu_map(adr);//
		palette = *src;

		if (x & 1) palette_entry = palette >> 4;
		else       palette_entry = palette & 0xF;
		color = LE_TO_LOCAL_16(pal[palette_entry]);

		// palette entry = 0 means backdrop
		if (palette_entry)
		{
         oam.set_pixel<true>(sprX, color, true, 16, prio, alpha ? 1 : 0);
		}
	}
}

INLINE void render_sprite_Win (GPU* gpu, u16 l, u32 srcadr,
	int col256, int lg, int sprX, int x, int xdir) {
	int i; u8 palette, palette_entry;
	u16 x1;
	u8* src = (u8 *)MMU_gpu_map(srcadr);

   // TODO: Don't draw if obj window is disabled!

   const u8 window_control = gpu->get_window_control(3);

	if (col256) {
		for(i = 0; i < lg; i++, sprX++,x+=xdir)
			if(src[(x&7) + ((x&0xFFF8)<<3)]) 
				gpu->window_map[sprX] = window_control;
	} else {
		for(i = 0; i < lg; i++, ++sprX, x+=xdir)
		{
			x1 = x>>1;
			palette = src[(x1&0x3) + ((x1&0xFFFC)<<3)];
			if (x & 1) palette_entry = palette >> 4;
			else       palette_entry = palette & 0xF;
			if(palette_entry)
				gpu->window_map[sprX] = window_control;
		}
	}
}

// return val means if the sprite is to be drawn or not
FORCEINLINE BOOL compute_sprite_vars(const oam_object_t* sprite, u16 l, 
	size &sprSize, s32 &sprX, s32 &sprY, s32 &x, s32 &y, s32 &lg, int &xdir) {

	x = 0;
	// get sprite location and size
	sprX = (sprite->X/*<<23*/)/*>>23*/;
	sprY = sprite->Y;
	sprSize = sprSizeTab[sprite->Size][sprite->Shape];

	lg = sprSize.x;
	
	if (sprY>=192)
		sprY = (s32)((s8)(sprite->Y));
	
// FIXME: for rot/scale, a list of entries into the sprite should be maintained,
// that tells us where the first pixel of a screenline starts in the sprite,
// and how a step to the right in a screenline translates within the sprite

	//this wasn't really tested by anything. very unlikely to get triggered
	y = (l - sprY)&255;                        /* get the y line within sprite coords */
	if(y >= sprSize.y)
		return FALSE;

	if((sprX==256)||(sprX+sprSize.x<=0))	/* sprite pixels outside of line */
		return FALSE;				/* not to be drawn */

	// sprite portion out of the screen (LEFT)
	if(sprX<0)
	{
		lg += sprX;	
		x = -(sprX);
		sprX = 0;
	}
	// sprite portion out of the screen (RIGHT)
	if (sprX+sprSize.x >= 256)
		lg = 256 - sprX;

	// switch TOP<-->BOTTOM
	if (sprite->VFlip)
		y = sprSize.y - y -1;
	
	// switch LEFT<-->RIGHT
	if (sprite->HFlip) {
		x = sprSize.x - x -1;
		xdir  = -1;
	} else {
		xdir  = 1;
	}
	return TRUE;
}

/**********************************/
// GPU::oam_t
/**********************************/
void GPU::oam_t::reset(GPU* parent_)
{
   parent          = parent_;
   mode            = SPRITE_1D;
   oam             = MMU.ARM9_OAM + ((parent->core == GPU_SUB) ? ADDRESS_STEP_1KB : 0);
   memory          = (parent->core == GPU_MAIN) ? MMU_AOBJ : MMU_BOBJ;
   enable          = false;

   boundary        = 0;
   bitmap_boundary = 0;

   visible_count   = 0;
   need_rebuild    = true;
}

bool GPU::oam_t::render_line()
{
   if (need_rebuild)
      rebuild();

   bool has_sprite = false;

	u16 l = parent->currLine;

	int cost = 0;

	const display_control_t display_control = parent->get_display_control();
   const oam_object_t* sprite_base = (const oam_object_t*)oam;

   memset(line_buffer, 0, sizeof(line_buffer));

	for(int t = 0; t != visible_count; t ++)
	{
      const oam_object_t* sprite = &sprite_base[visible_list[t]];

		//for each sprite:
		if(cost>=2130)
		{
			//out of sprite rendering time
			//printf("sprite overflow!\n");
			//return;		
		}

		//do we incur a cost if a sprite is disabled?? we guess so.
		cost += 2;

		size sprSize;
		s32 sprX, sprY, x, y, lg;
		int xdir;
		u8 prio, * src;
		u32 srcadr;
		u16 j;

		// Check if sprite is disabled before everything
		if (sprite->RotScale == 2)
			continue;

		prio = sprite->Priority;


		if (sprite->RotScale & 1) 
		{
			s32		fieldX, fieldY, auxX, auxY, realX, realY, offset;
			u8		blockparameter, *pal;
			s16		dx, dmx, dy, dmy;
			u16		colour;

			// Get sprite positions and size
			sprX = (sprite->X<<23)>>23;
			sprY = sprite->Y;
			sprSize = sprSizeTab[sprite->Size][sprite->Shape];

			lg = sprSize.x;
			
			if (sprY>=192)
				sprY = (s32)((s8)(sprite->Y));

			// Copy sprite size, to check change it if needed
			fieldX = sprSize.x;
			fieldY = sprSize.y;

			// If we are using double size mode, double our control vars
			if (sprite->RotScale & 2)
			{
				fieldX <<= 1;
				fieldY <<= 1;
				lg <<= 1;
			}

			//check if the sprite is visible y-wise. unfortunately our logic for x and y is different due to our scanline based rendering
			//tested thoroughly by many large sprites in Super Robot Wars K which wrap around the screen
			y = (l - sprY)&255;
			if(y >= fieldY)
				continue;

			//check if sprite is visible x-wise.
			if((sprX==256) || (sprX+fieldX<=0))
				continue;

			cost += sprSize.x*2 + 10;

			// Get which four parameter block is assigned to this sprite
			blockparameter = (sprite->RotScalIndex + (sprite->HFlip<< 3) + (sprite->VFlip << 4))*4;

			// Get rotation/scale parameters
			dx = SlurpOAMAffineParam(oam,blockparameter+0);
			dmx = SlurpOAMAffineParam(oam,blockparameter+1);
			dy = SlurpOAMAffineParam(oam,blockparameter+2);
			dmy = SlurpOAMAffineParam(oam,blockparameter+3);


			// Calculate fixed poitn 8.8 start offsets
			realX = ((sprSize.x) << 7) - (fieldX >> 1)*dx - (fieldY>>1)*dmx + y * dmx;
			realY = ((sprSize.y) << 7) - (fieldX >> 1)*dy - (fieldY>>1)*dmy + y * dmy;

			if(sprX<0)
			{
				// If sprite is not in the window
				if(sprX + fieldX <= 0)
					continue;

				// Otherwise, is partially visible
				lg += sprX;
				realX -= sprX*dx;
				realY -= sprX*dy;
				sprX = 0;
			}
			else
			{
				if(sprX+fieldX>256)
					lg = 256 - sprX;
			}

			// If we are using 1 palette of 256 colours
			if(sprite->Depth)
			{
				src = (u8 *)MMU_gpu_map(memory + (sprite->TileIndex << boundary));

				// If extended palettes are set, use them
				if (display_control.ExOBJPalette_Enable)
					pal = (MMU.ObjExtPal[parent->core][0]+(sprite->PaletteIndex*0x200));
				else
					pal = (MMU.ARM9_VMEM + 0x200 + parent->core *0x400);

				for(j = 0; j < lg; ++j, ++sprX)
				{
					// Get the integer part of the fixed point 8.8, and check if it lies inside the sprite data
					auxX = (realX>>8);
					auxY = (realY>>8);

					if (auxX >= 0 && auxY >= 0 && auxX < sprSize.x && auxY < sprSize.y)
					{
						if(mode == SPRITE_2D)
							offset = (auxX&0x7) + ((auxX&0xFFF8)<<3) + ((auxY>>3)<<10) + ((auxY&0x7)*8);
						else
							offset = (auxX&0x7) + ((auxX&0xFFF8)<<3) + ((auxY>>3)*sprSize.x*8) + ((auxY&0x7)*8);

						colour = src[offset];

                  has_sprite = true;

						if (colour)
						{ 
                     set_pixel<true>(sprX, HostReadWord(pal, (colour<<1)), true, 16, prio, sprite->Mode);
						}
					}

					//  Add the rotation/scale coeficients, here the rotation/scaling
					// is performed
					realX += dx;
					realY += dy;
				}

				continue;
			}
			// Rotozoomed direct color
			else if(sprite->Mode == 3)
			{
				//transparent (i think, dont bother to render?) if alpha is 0
				if(sprite->PaletteIndex == 0)
					continue;

				srcadr = sprite_address(*sprite, 0);

				for(j = 0; j < lg; ++j, ++sprX)
				{
					// Get the integer part of the fixed point 8.8, and check if it lies inside the sprite data
					auxX = (realX>>8);
					auxY = (realY>>8);

					//this is all very slow, and so much dup code with other rotozoomed modes.
					//dont bother fixing speed until this whole thing gets reworked

					if (auxX >= 0 && auxY >= 0 && auxX < sprSize.x && auxY < sprSize.y)
					{
						if(display_control.OBJ_BMP_2D_dim)
							//tested by knights in the nightmare
							offset = (sprite_address(*sprite, auxY)-srcadr)/2+auxX;
						else //tested by lego indiana jones (somehow?)
							//tested by buffy sacrifice damage blood splatters in corner
							offset = auxX + (auxY*sprSize.x);


						u16* mem = (u16*)MMU_gpu_map(srcadr + (offset<<1));
						
						colour = T1ReadWord(mem,0);

                  has_sprite = true;

						if(colour&0x8000)
						{
                     set_pixel<true>(sprX, colour, true, sprite->PaletteIndex, prio, sprite->Mode);
						}
					}

					//  Add the rotation/scale coeficients, here the rotation/scaling
					// is performed
					realX += dx;
					realY += dy;
				}

				continue;
			}
			// Rotozoomed 16/16 palette
			else
			{
				if(mode == SPRITE_2D)
				{
					src = (u8 *)MMU_gpu_map(memory + (sprite->TileIndex<<5));
					pal = MMU.ARM9_VMEM + 0x200 + (parent->core*0x400 + (sprite->PaletteIndex*32));
				}
				else
				{
					src = (u8 *)MMU_gpu_map(memory + (sprite->TileIndex<<boundary));
					pal = MMU.ARM9_VMEM + 0x200 + parent->core*0x400 + (sprite->PaletteIndex*32);
				}

            const u32 window_control = parent->get_window_control(3);

				for(j = 0; j < lg; ++j, ++sprX)
				{
					// Get the integer part of the fixed point 8.8, and check if it lies inside the sprite data
					auxX = (realX>>8);
					auxY = (realY>>8);

					if (auxX >= 0 && auxY >= 0 && auxX < sprSize.x && auxY < sprSize.y)
					{
						if(mode == SPRITE_2D)
							offset = ((auxX>>1)&0x3) + (((auxX>>1)&0xFFFC)<<3) + ((auxY>>3)<<10) + ((auxY&0x7)*4);
						else
							offset = ((auxX>>1)&0x3) + (((auxX>>1)&0xFFFC)<<3) + ((auxY>>3)*sprSize.x)*4 + ((auxY&0x7)*4);
						
						colour = src[offset];

						// Get 4bits value from the readed 8bits
						if (auxX&1)	colour >>= 4;
						else		colour &= 0xF;

                  has_sprite = true;

						if(colour && (prio < priority_at<true>(sprX)))
						{
							if(sprite->Mode==2)
								parent->window_map[sprX] = window_control;
							else
							{
                        set_pixel<true>(sprX, LE_TO_LOCAL_16(HostReadWord(pal, colour << 1)), true, 16, prio, sprite->Mode);
							}
						}
					}

					//  Add the rotation/scale coeficients, here the rotation/scaling
					// is performed
					realX += dx;
					realY += dy;
				}

				continue;
			}
		}
		else //NOT rotozoomed
		{
			u16 * pal;
	
			if (!compute_sprite_vars(sprite, l, sprSize, sprX, sprY, x, y, lg, xdir))
				continue;

			cost += sprSize.x;
         has_sprite = true;

         const u32 address = sprite_address(*sprite, y);

			if (sprite->Mode == 2)
			{
				render_sprite_Win (parent, l, address, sprite->Depth, lg, sprX, x, xdir);
				continue;
			}

			if (sprite->Mode == 3)              //sprite is in BMP format
			{
				//transparent (i think, dont bother to render?) if alpha is 0
				if(sprite->PaletteIndex == 0)
					continue;
				
				render_sprite_BMP (*this, l, address, prio, lg, sprX, x, xdir, sprite->PaletteIndex);
				continue;
			}
				
			if(sprite->Depth) //256 colors
			{				
				if (display_control.ExOBJPalette_Enable)
					pal = (u16*)(MMU.ObjExtPal[parent->core][0]+(sprite->PaletteIndex*0x200));
				else
					pal = (u16*)(MMU.ARM9_VMEM + 0x200 + parent->core *0x400);
		
				render_sprite_256(*this, l, address, pal, prio, lg, sprX, x, xdir, sprite->Mode == 1);

				continue;
			}

			// 16 colors 				
			pal = (u16*)(MMU.ARM9_VMEM + 0x200 + parent->core * 0x400);
			
			pal += (sprite->PaletteIndex<<4);
			
			render_sprite_16 (*this, l, address, pal, prio, lg, sprX, x, xdir, sprite->Mode == 1);
		}
	}

   return has_sprite;
}

void GPU::oam_t::rebuild()
{
   need_rebuild = false;
   visible_count = 0;

	const display_control_t display_control = parent->get_display_control();
   const oam_object_t* sprite = (const oam_object_t*)oam;

	for(int i = 0; i != 128; i ++, sprite ++)
   {
      const size& sprite_size = sprite->get_size();

      if (((sprite->Y < 192) || ((sprite->Y + sprite_size.y) < 256)) && ((sprite->X + sprite_size.x) > 0))
      {
         visible_list[visible_count ++] = i;
      }
   }
}

FORCEINLINE u32 GPU::oam_t::sprite_address(const oam_object_t& sprite, u32 line)
{
   const u32 width = sprite.get_size().x;

   if (sprite.Mode != 3) // Non-bitmap
   {
      const u32 line_size = sprite.Depth ? 8 : 4;

      if(mode == SPRITE_2D)
      {
         return memory + (sprite.TileIndex * 32)
                       + ((line / 8) * 1024)
                       + ((line & 7) * line_size);
      }
      else
      {
         return memory + (sprite.TileIndex << boundary)
                       + ((line / 8) * width * line_size)
                       + ((line & 7) * line_size);
      }
   }
   else
   {
      if (parent->get_display_control().OBJ_BMP_mapping)
      {
         //tested by buffy sacrifice damage blood splatters in corner
         return memory + (sprite.TileIndex << bitmap_boundary) + (line * width * 2);
      }
      else
      {
         //2d mapping:
         //verified in rotozoomed mode by knights in the nightmare intro

         if (parent->get_display_control().OBJ_BMP_2D_dim)
         {
            //256*256, verified by heroes of mana FMV intro
            return memory + (((sprite.TileIndex & 0x3E0) * 64  + (sprite.TileIndex & 0x1F) * 8 + ( line << 8)) << 1);
         }
         else 
         {
            //128*512, verified by harry potter and the order of the phoenix conversation portraits
            return memory + (((sprite.TileIndex&0x3F0) * 64  + (sprite.TileIndex&0x0F) *8 + ( line << 7)) << 1);
         }
      }
   }
}
