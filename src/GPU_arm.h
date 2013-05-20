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

#ifndef GPU_ARM_H
#define GPU_ARM_H

#include <stdio.h>
#include "mem.h"
#include "common.h"
#include "registers.h"
#include "FIFO.h"
#include "MMU.h"
#include <iosfwd>

//#undef FORCEINLINE
//#define FORCEINLINE

void gpu_savestate(EMUFILE* os);
bool gpu_loadstate(EMUFILE* is, int size);

/*******************************************************************************
    this structure is for display control,
    it holds flags for general display
*******************************************************************************/

union display_control_t
{
   u32 value;

   struct
   {
      unsigned BG_Mode              : 3; // A+B:
      unsigned BG0_3D               : 1; // A  : 0=2D,         1=3D
      unsigned OBJ_Tile_mapping     : 1; // A+B: 0=2D (32KB),  1=1D (32..256KB)
      unsigned OBJ_BMP_2D_dim       : 1; // A+B: 0=128x512,    1=256x256 pixels
      unsigned OBJ_BMP_mapping      : 1; // A+B: 0=2D (128KB), 1=1D (128..256KB)

                                   
      unsigned ForceBlank           : 1; // A+B:
      unsigned BG0_Enable           : 1; // A+B: 0=disable, 1=Enable
      unsigned BG1_Enable           : 1; // A+B: 0=disable, 1=Enable
      unsigned BG2_Enable           : 1; // A+B: 0=disable, 1=Enable
      unsigned BG3_Enable           : 1; // A+B: 0=disable, 1=Enable
      unsigned OBJ_Enable           : 1; // A+B: 0=disable, 1=Enable
      unsigned Win0_Enable          : 1; // A+B: 0=disable, 1=Enable
      unsigned Win1_Enable          : 1; // A+B: 0=disable, 1=Enable
      unsigned WinOBJ_Enable        : 1; // A+B: 0=disable, 1=Enable

      unsigned DisplayMode          : 2; // A+B: coreA(0..3) coreB(0..1) GBA(Green Swap)
                                         // 0=off (white screen)
                                         // 1=on (normal BG & OBJ layers)
                                         // 2=VRAM display (coreA only)
                                         // 3=RAM display (coreA only, DMA transfers)

      unsigned VRAM_Block           : 2; // A  : VRAM block (0..3=A..D)
      unsigned OBJ_Tile_1D_Bound    : 2; // A+B:
      unsigned OBJ_BMP_1D_Bound     : 1; // A  :
      unsigned OBJ_HBlank_process   : 1; // A+B: OBJ processed during HBlank (GBA bit5)
      unsigned CharacBase_Block     : 3; // A  : Character Base (64K step)
      unsigned ScreenBase_Block     : 3; // A  : Screen Base (64K step)
      unsigned ExBGxPalette_Enable  : 1; // A+B: 0=disable, 1=Enable BG extended Palette
      unsigned ExOBJPalette_Enable  : 1; // A+B: 0=disable, 1=Enable OBJ extended Palette
   } __attribute__((packed));
};

union background_control_t
{
   u16 value;

   struct
   {
      unsigned Priority             : 2; // 0..3=high..low
      unsigned CharacBase_Block     : 4; // individual character base offset (n*16KB)
      unsigned Mosaic_Enable        : 1; // 0=disable, 1=Enable mosaic
      unsigned Palette_256          : 1; // 0=16x16, 1=1*256 palette
      unsigned ScreenBase_Block     : 5; // individual screen base offset (text n*2KB, BMP n*16KB)
      unsigned PaletteSet_Wrap      : 1; // BG0 extended palette set 0=set0, 1=set2
                                         // BG1 extended palette set 0=set1, 1=set3
                                         // BG2 overflow area wraparound 0=off, 1=wrap
                                         // BG3 overflow area wraparound 0=off, 1=wrap
      unsigned ScreenSize           : 2; // text    : 256x256 512x256 256x512 512x512
                                         // x/rot/s : 128x128 256x256 512x512 1024x1024
                                         // bmp     : 128x128 256x256 512x256 512x512
                                         // large   : 512x1024 1024x512 - -
   } __attribute__((packed));
};

union background_offset_t
{
   u32 value;

   struct
   {
      unsigned x                    : 12;
      unsigned unused1              : 4;
      unsigned y                    : 12;
      unsigned unused2              : 4;
   } __attribute__((packed));
};

struct affine_parameters_t
{
   s16 PA;
   s16 PB;
   s16 PC;
   s16 PD;
   s32 X;
   s32 Y;
};

struct window_rects_t
{
   union
   {
      u32 x_values;

      struct
      {
         unsigned win_0_x2          : 8;
         unsigned win_0_x1          : 8;
         unsigned win_1_x2          : 8;
         unsigned win_1_x1          : 8;
      } __attribute__((packed));
   };

   union
   {
      u32 y_values;

      struct
      {
         unsigned win_0_y2          : 8;
         unsigned win_0_y1          : 8;
         unsigned win_1_y2          : 8;
         unsigned win_1_y1          : 8;
      } __attribute__((packed));
   };
};

union blend_control_t
{
   u16 value;

   struct
   {
      unsigned first_target_mask    : 6;
      unsigned effect               : 2;
      unsigned second_target_mask   : 6;
      unsigned                      : 2;
   } __attribute__((packed));
};

union blend_alpha_t
{
   u16 value;

   u32 get_first_target_factor()  { return first_target_max  ? 16 : first_target_factor;  }
   u32 get_second_target_factor() { return second_target_max ? 16 : second_target_factor; }

   struct
   {
      unsigned first_target_factor  : 4;
      unsigned first_target_max     : 1;
      unsigned                      : 3;
      unsigned second_target_factor : 4;
      unsigned second_target_max    : 1;
      unsigned                      : 3;
   } __attribute__((packed));
};

union brightness_t
{
   u16 value;

   struct
   {
      unsigned factor               : 4;
      unsigned max                  : 1;
      unsigned                      : 11;
   } __attribute__((packed));
};

union master_bright_t
{
   u16 value;

   struct
   {
      unsigned factor               : 4;
      unsigned max                  : 1;
      unsigned unused               : 9;
      unsigned mode                 : 2;
   };
};


enum BlendFunc
{
	NoBlend, Blend, Increase, Decrease
};


/*******************************************************************************
    this structure is for 3D settings
*******************************************************************************/

struct _DISP3DCNT
{
/* 0*/ u8 EnableTexMapping:1;    //
/* 1*/ u8 PolygonShading:1;      // 0=Toon Shading, 1=Highlight Shading
/* 2*/ u8 EnableAlphaTest:1;     // see ALPHA_TEST_REF
/* 3*/ u8 EnableAlphaBlending:1; // see various Alpha values
/* 4*/ u8 EnableAntiAliasing:1;  //
/* 5*/ u8 EnableEdgeMarking:1;   // see EDGE_COLOR
/* 6*/ u8 FogOnlyAlpha:1;        // 0=Alpha and Color, 1=Only Alpha (see FOG_COLOR)
/* 7*/ u8 EnableFog:1;           // Fog Master Enable
/* 8*/ u8 FogShiftSHR:4;         // 0..10 SHR-Divider (see FOG_OFFSET)
/*12*/ u8 AckColorBufferUnderflow:1; // Color Buffer RDLINES Underflow (0=None, 1=Underflow/Acknowledge)
/*13*/ u8 AckVertexRAMOverflow:1;    // Polygon/Vertex RAM Overflow    (0=None, 1=Overflow/Acknowledge)
/*14*/ u8 RearPlaneMode:1;       // 0=Blank, 1=Bitmap
/*15*/ u8 :1;
/*16*/ u16 :16;
};

typedef union
{
    struct _DISP3DCNT bits;
    u32 val;
} DISP3DCNT;

/*******************************************************************************
    this structure is for capture control (core A only)

    source:
    http://nocash.emubase.de/gbatek.htm#dsvideocaptureandmainmemorydisplaymode
*******************************************************************************/
struct DISPCAPCNT
{
	enum CAPX {
		_128, _256
	} capx;
    u32 val;
	BOOL enabled;
	u8 EVA;
	u8 EVB;
	u8 writeBlock;
	u8 writeOffset;
	u16 capy;
	u8 srcA;
	u8 srcB;
	u8 readBlock;
	u8 readOffset;
	u8 capSrc;
} ;


/*******************************************************************************
    this structure holds everything and should be mapped to
    * core A : 0x04000000
    * core B : 0x04001000
*******************************************************************************/

struct REG_DISPx {
    display_control_t         display_control;        // 0x0400x000
    u16                       display_status;         // 0x04000004  (Unused, handled elsewhere, A only)
    u16                       vertical_counter;       // 0x0400x006  (Unused, handled elsewhere)
    background_control_t      background_control[4];  // 0x0400x008
    background_offset_t       background_offset[4];   // 0x0400x010
    affine_parameters_t       affine_parameters[2];   // 0x0400x020
    window_rects_t            window_rects;           // 0x0400x040
    u8                        window_control[4];      // 0x0400x048
    u16                       mosaic_size;            // 0x0400x04C
    u16                       unused;                 // 0x0400x04E
    blend_control_t           blend_control;          // 0x0400x050
    blend_alpha_t             blend_alpha;            // 0x0400x052
    brightness_t              brightness;             // 0x0400x054
    u16                       unused2[5];             // 0x0400x056
    DISP3DCNT                 dispA_DISP3DCNT;        // 0x04000060
    u32                       dispA_DISPCAPCNT;       // 0x04000064
    u32                       dispA_DISPMMEMFIFO;     // 0x04000068
    master_bright_t           master_bright;          // 0x0400x06C
} __attribute__((packed)) ;


typedef BOOL (*fun_gl_Begin) (int screen);
typedef void (*fun_gl_End) (int screen);
// the GUI should use this function prior to all gl calls
// if call to beg succeeds opengl draw
void register_gl_fun(fun_gl_Begin beg,fun_gl_End end);

#define GPU_MAIN	0
#define GPU_SUB		1

/* human readable bitmask names */
#define ADDRESS_STEP_512B	   0x00200
#define ADDRESS_STEP_1KB		0x00400
#define ADDRESS_STEP_2KB		0x00800
#define ADDRESS_STEP_4KB		0x01000
#define ADDRESS_STEP_8KB		0x02000
#define ADDRESS_STEP_16KB	   0x04000
#define ADDRESS_STEP_32KB	   0x08000
#define ADDRESS_STEP_64KB	   0x10000
#define ADDRESS_STEP_128KB	   0x20000
#define ADDRESS_STEP_256KB	   0x40000
#define ADDRESS_STEP_512KB	   0x80000
#define ADDRESS_MASK_256KB	   (ADDRESS_STEP_256KB-1)

/*
	this structure is for color representation,
	it holds 5 meaningful bits per color channel (red,green,blue)
	and	  1 meaningful bit for alpha representation
	this bit can be unused or used for special FX
*/

struct _COLOR { // abgr x555
     unsigned red:5;
     unsigned green:5;
     unsigned blue:5;
     unsigned alpha:1;    // sometimes it is unused (pad)
};
struct _COLORx { // abgr x555
	unsigned bgr:15;
	unsigned alpha:1;	// sometimes it is unused (pad)
};

typedef union
{
	struct _COLOR bits;
	struct _COLORx bitx;
	u16 val;
} COLOR;

struct _COLOR32 { // ARGB
	unsigned :3;
	unsigned blue:5;
	unsigned :3;
	unsigned green:5;
	unsigned :3;
	unsigned red:5;
	unsigned :7;
	unsigned alpha:1;	// sometimes it is unused (pad)
};

typedef union
{
	struct _COLOR32 bits;
	u32 val;
} COLOR32;

#define COLOR_16_32(w,i)	\
	/* doesnt matter who's 16bit who's 32bit */ \
	i.bits.red   = w.bits.red; \
	i.bits.green = w.bits.green; \
	i.bits.blue  = w.bits.blue; \
	i.bits.alpha = w.bits.alpha;



 // (00: Normal, 01: Transparent, 10: Object window, 11: Bitmap)
enum GPU_OBJ_MODE
{
	GPU_OBJ_MODE_Normal = 0,
	GPU_OBJ_MODE_Transparent = 1,
	GPU_OBJ_MODE_Window = 2,
	GPU_OBJ_MODE_Bitmap = 3
};


#define NB_PRIORITIES	4
#define NB_BG		4
//this structure holds information for rendering.
typedef struct
{
	u8 PixelsX[256];
	u8 BGs[NB_BG], nbBGs;
	u8 pad[1];
	u16 nbPixelsX;
	//256+8:
	u8 pad2[248];

	//things were slower when i organized this struct this way. whatever.
	//u8 PixelsX[256];
	//int BGs[NB_BG], nbBGs;
	//int nbPixelsX;
	////<-- 256 + 24
	//u8 pad2[256-24];
} itemsForPriority_t;
#define MMU_ABG		0x06000000
#define MMU_BBG		0x06200000
#define MMU_AOBJ	0x06400000
#define MMU_BOBJ	0x06600000
#define MMU_LCDC	0x06800000

extern CACHE_ALIGN u8 gpuBlendTable555[17][17][32][32];

struct PIXEL
{
   unsigned color    : 16;
   unsigned opaque   : 1;
   unsigned alpha    : 4;    // For sprites
   unsigned priority : 3; // For sprites
   unsigned type     : 2;     // For sprites
   unsigned pad      : 6;
};

FORCEINLINE PIXEL BuildPIXEL(u16 color, bool opaque)
{
   PIXEL result;
   result.color = color;
   result.opaque = opaque;
   return result;
}

FORCEINLINE PIXEL BuildPIXEL(u16 color, bool opaque, unsigned alpha, unsigned priority, unsigned type)
{
   PIXEL result;
   result.color = color;
   result.opaque = opaque;
   result.alpha = alpha;
   result.priority = priority;
   result.type = type;
   return result;
}

union oam_object_t;
struct size;

struct GPU
{
   struct background
   {
      enum bg_type
      {
         Invalid,   Text,             Affine,          Large8bpp, 
         AffineExt, AffineExt_256x16, AffineExt_256x1, AffineExt_Direct
      };
      static const bg_type background_types[8][4];

      public:
         void set_size(u32 width_, u32 height_) { width = width_; height = height_; }

         background_control_t get_control() const { return parent->dispx_st->background_control[number]; }
         void refresh_control();

         const u16* get_extended_palette() const { return parent->dispx_st->display_control.ExBGxPalette_Enable ? (u16*)MMU.ExtPal[parent->core][extended_palette_slot] : 0; }

         bool render_pixels(u32 line); // Only 8-262 are drawn

         u32 get_x_offset() const { return parent->dispx_st->background_offset[number].x; }
         u32 get_y_offset() const { return parent->dispx_st->background_offset[number].y; }
         affine_parameters_t& get_affine_parameters() { return parent->dispx_st->affine_parameters[(number == 2) ? 0 : 1]; }

      public: // Inlines
         // The CHECK template argument enables or disables bounds checking
         template <bool CHECK>
         FORCEINLINE void set_pixel(s32 x, PIXEL pixel)
         {
            if (!CHECK || (x >= 0 && x < 256))
            {
               if (pixel.opaque)
               {
                  parent->set_bg_color(pixel.color, x);
               }
            }
         }


      public:
         GPU* parent;
         u32 number;
         bg_type type;

         u32 tile_map_ram;
         u32 tile_pixel_ram;
         u32 bitmap_ram;
         u32 large_bitmap_ram;

         u32 extended_palette_slot;

         u32 width;
         u32 height;
   };

   struct oam_t
   {
      public:
         void reset(GPU* parent_);

         bool render_line();

         FORCEINLINE void invalidate() { need_rebuild = true; }
         void rebuild();

         FORCEINLINE u32 sprite_address(const oam_object_t& sprite, u32 line);

      public: // Inlines

         // The CHECK template argument enables or disables bounds checking
         // The priority field of the PIXEL value is set to 4 + priority, for performance reasons
         template <bool CHECK>
         FORCEINLINE void set_pixel(s32 x, u16 color, bool opaque, unsigned alpha, unsigned priority, unsigned type)
         {
            if (!CHECK || (x >= 0 && x < 256))
            {
               if (priority < (line_buffer[x].priority - 4))
               {
                  line_buffer[x] = BuildPIXEL(color, opaque, alpha, 4 + priority, type);
               }
            }
         }

         template <bool CHECK>
         FORCEINLINE u32 priority_at(s32 x) const
         {
            return (!CHECK || (x >= 0 && x < 256)) ? line_buffer[x].priority - 4 : 0xFFFFFFFF;
         }

      public:
         GPU* parent;
         bool enable;

         u32 visible_list[128];
         u32 visible_count;
         bool need_rebuild;

         const void* oam;
         u32 memory;

         u32 boundary;
         u32 bitmap_boundary;

         PIXEL line_buffer[256];

      	enum object_mode_t { SPRITE_1D, SPRITE_2D } mode;

   };

   public:
      void refresh_display_control();
      void refresh_background_control(u32 bg_number) { backgrounds[bg_number].refresh_control(); }
      void resort_backgrounds();

   	void calculate_windows();
   	bool check_window(u32 x, bool &effect) const;
      u32 get_window_control(u32 window) const { return dispx_st->window_control[window & 3]; }

      background& get_current_background() { return backgrounds[currBgNum]; }
      display_control_t get_display_control() const { return dispx_st->display_control; }
      blend_alpha_t get_blend_alpha() const { return dispx_st->blend_alpha; }
      brightness_t get_brightness() const { return dispx_st->brightness; }

   public: // Pixel rendering
      template<BlendFunc FUNC, bool WINDOW>
      FORCEINLINE FASTCALL void master_set_3d_color(int dstX, int srcX);
      FORCEINLINE FASTCALL void set_3d_color(int dstX, int srcX);

      template<BlendFunc FUNC, bool WINDOW>
      FORCEINLINE FASTCALL void master_set_bg_color(u16 color, const u32 x);
      FORCEINLINE FASTCALL void set_bg_color(u16 color, const u32 x);

      template<BlendFunc FUNC, bool WINDOW>
      FORCEINLINE FASTCALL void master_set_obj_color(u16 color, u8 alpha, u8 type, u16 x);
      FORCEINLINE FASTCALL void set_obj_color(u16 color, u8 alpha, u8 type, u16 x);

      FORCEINLINE FASTCALL void render_backdrop(u16 color);
      FORCEINLINE FASTCALL void render_3d_line(u32 line);

      int setFinalColorBck_funcNum;
      int setFinalColor3d_funcNum;
      int setFinalColorSpr_funcNum;

   public:
      background backgrounds[4];
      oam_t oam;

      u16* palette;
   	u8 window_map[256];	


   public:
	// some structs are becoming redundant
	// some functions too (no need to recopy some vars as it is done by MMU)
	REG_DISPx * dispx_st;

	void modeRender(int layer);

	DISPCAPCNT dispCapCnt;
	BOOL LayersEnable[5];
	itemsForPriority_t itemsForPriority[NB_PRIORITIES];


	u8 core;

	//FIFO	fifo;

	u8 WIN0_ENABLED;
	u8 WIN1_ENABLED;
	u8 WINOBJ_ENABLED;

	u16 BLDCNT;
	u16 *currentFadeInColors, *currentFadeOutColors;
	bool blend2[8];

	CACHE_ALIGN u16 tempScanlineBuffer[256];

	CACHE_ALIGN u8 bgPixels[1024]; //yes indeed, this is oversized. map debug tools try to write to it

	u32 currLine;
	u8 currBgNum;
	bool blend1;
	u8* currDst;

	u8* _3dColorLine;

	u16 blend(u16 colA, u16 colB);

	void setAffineStart(int layer, int xy, u32 val);
	void setAffineStartWord(int layer, int xy, u16 val, int word);
	u32 getAffineStart(int layer, int xy);
	void refreshAffineStartRegs(const int num, const int xy);

	struct AffineInfo {
		AffineInfo() : x(0), y(0) {}
		u32 x, y;
	} affineInfo[2];

	typedef u8 TBlendTable[32][32];
	TBlendTable *blendTable;
};

CACHE_ALIGN extern u8 GPU_screen[4*256*192];


GPU * GPU_Init(u8 l);
void GPU_Reset(GPU *g, u8 l);
void GPU_DeInit(GPU *);

typedef struct {
	GPU * gpu;
	u16 offset;
} NDS_Screen;

extern NDS_Screen MainScreen;
extern NDS_Screen SubScreen;

int Screen_Init();
void Screen_Reset(void);
void Screen_DeInit(void);

extern MMU_struct MMU;

void GPU_set_DISPCAPCNT(u32 val) ;
void GPU_RenderLine(NDS_Screen * screen, u16 l, bool skip = false) ;

// Blending
void SetupFinalPixelBlitter (GPU *gpu);
#define GPU_setBLDCNT_LOW(gpu, val)  {gpu->BLDCNT = (gpu->BLDCNT&0xFF00) | (val); SetupFinalPixelBlitter (gpu);}
#define GPU_setBLDCNT_HIGH(gpu, val) {gpu->BLDCNT = (gpu->BLDCNT&0xFF) | (val<<8); SetupFinalPixelBlitter (gpu);}
#define GPU_setBLDCNT(gpu, val)      {gpu->BLDCNT = (val); SetupFinalPixelBlitter (gpu);}

#endif

