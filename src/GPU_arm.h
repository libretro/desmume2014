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
    this structure is for miscellanous settings
    //TODO: needs further description
*******************************************************************************/

typedef struct {
    u16 unused1;
    u16 unused2;//BLDCNT;
    u16 unused3;//BLDALPHA;
    u16 unused4;//BLDY;
    u16 unused5;
    u16 unused6;
    u16 unused7;
    u16 unused8;
    u16 unused9;
} MISCCNT;


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
    u8                        filler[4];              // 0x0400x048
    u16                       mosaic_size;            // 0x0400x04C
    MISCCNT                   dispx_MISC;             // 0x0400x04E
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

struct _TILEENTRY
{
/* 0*/	unsigned TileNum:10;
/*12*/	unsigned HFlip:1;	// HORIZONTAL FLIP (left<-->right)
/*13*/	unsigned VFlip:1;	// VERTICAL FLIP (top<-->bottom)
/*14*/	unsigned Palette:4;
};
typedef union
{
	struct _TILEENTRY bits;
	u16 val;
} TILEENTRY;

union ROTOCOORD
{
   ROTOCOORD(s32 value_) : val(value_) { }

   s32 val;

   struct
   {
      unsigned Fraction : 8;
      signed   Integer  : 20;
      unsigned pad      : 4;
   } __attribute__((packed));
};

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

union _OAM_tag
{
   u16 attr[4];

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
typedef const _OAM_tag _OAM_;

_OAM_* SlurpOAM(void* oam_buffer, int oam_index);
u16 SlurpOAMAffineParam(void* oam_buffer, int oam_index);

typedef struct
{
	 s16 x;
	 s16 y;
} size;


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

enum BGType {
	BGType_Invalid=0, BGType_Text=1, BGType_Affine=2, BGType_Large8bpp=3, 
	BGType_AffineExt=4, BGType_AffineExt_256x16=5, BGType_AffineExt_256x1=6, BGType_AffineExt_Direct=7
};

struct GPU
{
   struct background
   {
      public:
         void set_size(u32 width_, u32 height_) { width = width_; height = height_; }
         background_control_t get_control() const { return parent->dispx_st->background_control[number]; }

      public:
         GPU* parent;
         u32 number;
         BGType type;

         u32 tile_map_ram;
         u32 tile_pixel_ram;
         u32 bitmap_ram;
         u32 large_bitmap_ram;

         u32 extended_palette_slot;

         u32 width;
         u32 height;
   };

   public:
      void refresh_display_control();
      void refresh_background_control(u32 bg_number);
      void resort_backgrounds();

      void force_window_h_refresh(u32 window_number) { need_update_winh[window_number ? 1 : 0] = true; }

      background& get_current_background() { return backgrounds[currBgNum]; }
      display_control_t& get_display_control() const { return dispx_st->display_control; }
      affine_parameters_t& get_affine_parameters_for_bg(u32 bg) { return dispx_st->affine_parameters[(bg == 2) ? 0 : 1]; }

   public:
      bool need_update_winh[2];
      background backgrounds[4];

   public:
	// some structs are becoming redundant
	// some functions too (no need to recopy some vars as it is done by MMU)
	REG_DISPx * dispx_st;

	void modeRender(int layer);

	DISPCAPCNT dispCapCnt;
	BOOL LayersEnable[5];
	itemsForPriority_t itemsForPriority[NB_PRIORITIES];


   // 
	u8 h_win[2][256];
	const u8 *curr_win[2];
	void update_winh(int WIN_NUM); 

	
	template<int WIN_NUM> void setup_windows();

	u8 core;

	//FIFO	fifo;

	void * oam;
	u32	sprMem;
	u8 sprBoundary;
	u8 sprBMPBoundary;
	u32 sprEnable;

	u8 WININ0;
	bool WININ0_SPECIAL;
	u8 WININ1;
	bool WININ1_SPECIAL;

	u8 WINOUT;
	bool WINOUT_SPECIAL;
	u8 WINOBJ;
	bool WINOBJ_SPECIAL;

	u8 WIN0_ENABLED;
	u8 WIN1_ENABLED;
	u8 WINOBJ_ENABLED;

	u16 BLDCNT;
	u8	BLDALPHA_EVA;
	u8	BLDALPHA_EVB;
	u8	BLDY_EVY;
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

	template<bool BACKDROP, BlendFunc FUNC, bool WINDOW>
	FORCEINLINE FASTCALL bool _master_setFinalBGColor(u16 &color, const u32 x);

	template<BlendFunc FUNC, bool WINDOW>
	FORCEINLINE FASTCALL void _master_setFinal3dColor(int dstX, int srcX);

	int setFinalColorBck_funcNum;
	int setFinalColor3d_funcNum;
	int setFinalColorSpr_funcNum;
	//Final3DColFunct setFinalColor3D;
	enum SpriteRenderMode {
		SPRITE_1D, SPRITE_2D
	} spriteRenderMode;

	template<GPU::SpriteRenderMode MODE>
	bool _spriteRender(u8 * dst, u8 * dst_alpha, u8 * typeTab, u8 * prioTab);
	
	inline bool spriteRender(u8 * dst, u8 * dst_alpha, u8 * typeTab, u8 * prioTab)
	{
      return (spriteRenderMode == SPRITE_1D) ? _spriteRender<SPRITE_1D>(dst,dst_alpha,typeTab, prioTab)
                                             : _spriteRender<SPRITE_2D>(dst,dst_alpha,typeTab, prioTab);
	}


	void setFinalColor3d(int dstX, int srcX);
	
	template<bool BACKDROP, int FUNCNUM> void setFinalColorBG(u16 color, const u32 x);
	template<bool BACKDROP> FORCEINLINE void __setFinalColorBck(u16 color, const u32 x, const int opaque);
	template<bool BACKDROP, int FUNCNUM> FORCEINLINE void ___setFinalColorBck(u16 color, const u32 x, const int opaque);

	void setAffineStart(int layer, int xy, u32 val);
	void setAffineStartWord(int layer, int xy, u16 val, int word);
	u32 getAffineStart(int layer, int xy);
	void refreshAffineStartRegs(const int num, const int xy);

	struct AffineInfo {
		AffineInfo() : x(0), y(0) {}
		u32 x, y;
	} affineInfo[2];

	void renderline_checkWindows(u16 x, bool &draw, bool &effect) const;

	// check whether (x,y) is within the rectangle (including wraparounds) 
	template<int WIN_NUM>
	u8 withinRect(u16 x) const;

	void setBLDALPHA(u16 val)
	{
		BLDALPHA_EVA = (val&0x1f) > 16 ? 16 : (val&0x1f); 
		BLDALPHA_EVB = ((val>>8)&0x1f) > 16 ? 16 : ((val>>8)&0x1f);
		updateBLDALPHA();
	}

	void setBLDALPHA_EVA(u8 val)
	{
		BLDALPHA_EVA = (val&0x1f) > 16 ? 16 : (val&0x1f);
		updateBLDALPHA();
	}
	
	void setBLDALPHA_EVB(u8 val)
	{
		BLDALPHA_EVB = (val&0x1f) > 16 ? 16 : (val&0x1f);
		updateBLDALPHA();
	}

	u32 getHOFS(int bg) { return dispx_st->background_offset[bg].x; }
	u32 getVOFS(int bg) { return dispx_st->background_offset[bg].y; }

	typedef u8 TBlendTable[32][32];
	TBlendTable *blendTable;

	void updateBLDALPHA()
	{
		blendTable = (TBlendTable*)&gpuBlendTable555[BLDALPHA_EVA][BLDALPHA_EVB][0][0];
	}
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

void GPU_setBLDCNT(GPU *gpu, u16 v) ;
void GPU_setBLDY(GPU *gpu, u16 v) ;

int GPU_ChangeGraphicsCore(int coreid);

void GPU_set_DISPCAPCNT(u32 val) ;
void GPU_RenderLine(NDS_Screen * screen, u16 l, bool skip = false) ;

inline void GPU_setWININ(GPU* gpu, u16 val) {
	gpu->WININ0=val&0x1F;
	gpu->WININ0_SPECIAL=((val>>5)&1)!=0;
	gpu->WININ1=(val>>8)&0x1F;
	gpu->WININ1_SPECIAL=((val>>13)&1)!=0;
}

inline void GPU_setWININ0(GPU* gpu, u8 val) { gpu->WININ0 = val&0x1F; gpu->WININ0_SPECIAL = (val>>5)&1; }
inline void GPU_setWININ1(GPU* gpu, u8 val) { gpu->WININ1 = val&0x1F; gpu->WININ1_SPECIAL = (val>>5)&1; }

inline void GPU_setWINOUT16(GPU* gpu, u16 val) {
	gpu->WINOUT=val&0x1F;
	gpu->WINOUT_SPECIAL=((val>>5)&1)!=0;
	gpu->WINOBJ=(val>>8)&0x1F;
	gpu->WINOBJ_SPECIAL=((val>>13)&1)!=0;
}
inline void GPU_setWINOUT(GPU* gpu, u8 val) { gpu->WINOUT = val&0x1F; gpu->WINOUT_SPECIAL = (val>>5)&1; }
inline void GPU_setWINOBJ(GPU* gpu, u8 val) { gpu->WINOBJ = val&0x1F; gpu->WINOBJ_SPECIAL = (val>>5)&1; }

// Blending
void SetupFinalPixelBlitter (GPU *gpu);
#define GPU_setBLDCNT_LOW(gpu, val) {gpu->BLDCNT = (gpu->BLDCNT&0xFF00) | (val); SetupFinalPixelBlitter (gpu);}
#define GPU_setBLDCNT_HIGH(gpu, val) {gpu->BLDCNT = (gpu->BLDCNT&0xFF) | (val<<8); SetupFinalPixelBlitter (gpu);}
#define GPU_setBLDCNT(gpu, val) {gpu->BLDCNT = (val); SetupFinalPixelBlitter (gpu);}



#define GPU_setBLDY_EVY(gpu, val) {gpu->BLDY_EVY = ((val)&0x1f) > 16 ? 16 : ((val)&0x1f);}

#endif

