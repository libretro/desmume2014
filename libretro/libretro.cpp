#include "libretro.h"

#include "MMU.h"
#include "NDSSystem.h"
#include "debug.h"
#include "sndsdl.h"
#include "render3D.h"
#include "rasterize.h"
#include "saves.h"
#include "firmware.h"
#include "GPU_osd.h"
#include "addons.h"

//

static retro_video_refresh_t video_cb = NULL;
static retro_input_poll_t poll_cb = NULL;
static retro_input_state_t input_cb = NULL;
static retro_audio_sample_batch_t audio_batch_cb = NULL;
retro_environment_t environ_cb = NULL;

//

volatile bool execute = false;

// Video buffer
static uint8_t screenSwap[256 * 192 * 2 * 4];

#ifndef SWAPTYPE
# define SWAPTYPE uint32_t
#endif

namespace VIDEO
{
    retro_pixel_format colorMode;

    void SwapScreen32(uint32_t* aOut, const uint16_t* aIn, uint32_t aPitchInPix)
    {    
        for(int i = 0; i != 192; i ++)
        {
            uint32_t* out = (uint32_t*)&aOut[i * aPitchInPix];
        
            for(int j = 0; j != 256; j ++)
            {
                const uint16_t p = *aIn++;
                const uint32_t r = (p & 0x1F) << 19;
                const uint32_t g = ((p >> 5) & 0x1F) << 11;
                const uint32_t b = ((p >> 10) & 0x1F) << 3;
                *out++ = r | g | b;
            }
        }
    }
    
    template<typename T, unsigned EXTRA>
    void SwapScreen16(void* aOut, const void* aIn, uint32_t aPitchInPix)
    {
        static const uint32_t pixPerT = sizeof(T) / 2;
        static const uint32_t pixPerLine = 256 / pixPerT;
        static const T colorMask = (pixPerT == 1) ? 0x001F : ((pixPerT == 2) ? 0x001F001F : 0x001F001F001F001F);
        
        assert(pixPerT == 1 || pixPerT == 2 || pixPerT == 4);
        
        const uint32_t pitchInT = aPitchInPix / pixPerT;
        const T* inPix = (const T*)aIn;
        
        for(int i = 0; i != 192; i ++)
        {
            T* outPix = (T*)aOut + (i * pitchInT);
        
            for(int j = 0; j != pixPerLine; j ++)
            {
                const T p = *inPix++;            
                const T r = ((p >> 10) & colorMask);
                const T g = ((p >> 5) & colorMask) << 5 + EXTRA;
                const T b = ((p >> 0) & colorMask) << 10 + EXTRA;
                *outPix++ = r | g | b;
            }
        }
    }
}

namespace AUDIO
{
    static unsigned frames;
    
    int SNDRetroInit(int buffersize){return 0;}
    void SNDRetroDeInit(){}
    void SNDRetroMuteAudio(){}
    void SNDRetroUnMuteAudio(){}
    void SNDRetroSetVolume(int volume){}
    
    
    void SNDRetroUpdateAudio(s16 *buffer, u32 num_samples)
    {
        audio_batch_cb(buffer, num_samples);
        frames += num_samples;
    }
    
    u32 SNDRetroGetAudioSpace(){return 735 - frames;}
    
    const int SNDCORE_RETRO = 2000;
    SoundInterface_struct SNDRetro =
    {
        SNDCORE_RETRO,
        "libretro Sound Interface",
        SNDRetroInit,
        SNDRetroDeInit,
        SNDRetroUpdateAudio,
        SNDRetroGetAudioSpace,
        SNDRetroMuteAudio,
        SNDRetroUnMuteAudio,
        SNDRetroSetVolume
    };
}

namespace INPUT
{
    template<int min, int max>
    static int32_t ClampedMove(int32_t aTarget, int32_t aValue)
    {
        return std::max(min, std::min(max, aTarget + aValue));
    }

    static int32_t TouchX;
    static int32_t TouchY;
    
    static const uint8_t CursorImage[16*8] =
    {
        2, 0, 0, 0, 0, 0, 0, 0, 
        2, 2, 0, 0, 0, 0, 0, 0, 
        2, 1, 2, 0, 0, 0, 0, 0, 
        2, 1, 1, 2, 0, 0, 0, 0, 
        2, 1, 1, 1, 2, 0, 0, 0, 
        2, 1, 1, 1, 1, 2, 0, 0, 
        2, 1, 1, 1, 1, 1, 2, 0, 
        2, 1, 1, 1, 1, 1, 1, 2, 
        2, 1, 1, 1, 1, 1, 2, 2, 
        2, 1, 1, 1, 1, 2, 0, 0, 
        2, 1, 2, 1, 1, 2, 0, 0, 
        2, 2, 0, 2, 1, 2, 0, 0, 
        2, 0, 0, 0, 2, 1, 2, 0, 
        0, 0, 0, 0, 2, 1, 2, 0, 
        0, 0, 0, 0, 0, 2, 1, 2, 
        0, 0, 0, 0, 0, 2, 2, 2, 
    };

    unsigned Devices[2] = {RETRO_DEVICE_JOYPAD, RETRO_DEVICE_MOUSE};

    void DrawPointer32(uint32_t* aOut, uint32_t aPitchInPix)
    {
        // Draw pointer
        if(INPUT::Devices[1] == RETRO_DEVICE_MOUSE)
        {
            for(int i = 0; i != 16 && INPUT::TouchY + i < 192; i ++)
            {
                for(int j = 0; j != 8 && INPUT::TouchX + j < 256; j ++)
                {
                    if(INPUT::CursorImage[i * 8 + j])
                    {
                        aOut[(i + INPUT::TouchY) * aPitchInPix + INPUT::TouchX + j] = 0xFFFFFF;
                    }
                }
            }
        }    
    }
    
    template<unsigned COLOR>
    void DrawPointer16(uint16_t* aOut, uint32_t aPitchInPix)
    {
        // Draw pointer
        if(INPUT::Devices[1] == RETRO_DEVICE_MOUSE)
        {
            for(int i = 0; i != 16 && INPUT::TouchY + i < 192; i ++)
            {
                for(int j = 0; j != 8 && INPUT::TouchX + j < 256; j ++)
                {
                    if(INPUT::CursorImage[i * 8 + j])
                    {
                        aOut[(i + INPUT::TouchY) * aPitchInPix + INPUT::TouchX + j] = COLOR;
                    }
                }
            }
        }    
    }
}

SoundInterface_struct* SNDCoreList[] =
{
	&SNDDummy,
	&AUDIO::SNDRetro,
	NULL
};

GPU3DInterface* core3DList[] =
{
	&gpu3DRasterize,
	NULL
};



//

void *retro_get_memory_data(unsigned type)
{
   return 0;
}

size_t retro_get_memory_size(unsigned type)
{
   return 0;
}

unsigned retro_api_version(void)
{
   return RETRO_API_VERSION;
}

void retro_set_video_refresh(retro_video_refresh_t cb)
{
   video_cb = cb;
}

void retro_set_audio_sample(retro_audio_sample_t cb)
{}

void retro_set_audio_sample_batch(retro_audio_sample_batch_t cb)
{
   audio_batch_cb = cb;
}

void retro_set_input_poll(retro_input_poll_t cb)
{
   poll_cb = cb;
}

void retro_set_input_state(retro_input_state_t cb)
{
   input_cb = cb;
}

void retro_set_environment(retro_environment_t cb)
{
   environ_cb = cb;
}

void retro_get_system_info(struct retro_system_info *info)
{
   info->library_name = "DeSmuME";
   info->library_version = "svn";
   info->valid_extensions = "nds";
   info->need_fullpath = true;   
   info->block_extract = false;
}

void retro_set_controller_port_device(unsigned in_port, unsigned device)
{
    if(in_port < 2)
    {
        INPUT::Devices[in_port] = device;
    }
}

void retro_get_system_av_info(struct retro_system_av_info *info)
{
    // TODO
    info->geometry.base_width = 256;
    info->geometry.base_height = 192*2;
    info->geometry.max_width = 256;
    info->geometry.max_height = 192*2;
    info->geometry.aspect_ratio = 256.0 / (192.0 * 2.0);
    info->timing.fps = 60.0;
    info->timing.sample_rate = 44100.0;
}

void retro_init (void)
{
    // Get Color 32->16->15
    VIDEO::colorMode = RETRO_PIXEL_FORMAT_XRGB8888;
    if(!environ_cb(RETRO_ENVIRONMENT_SET_PIXEL_FORMAT, &VIDEO::colorMode))
    {
        VIDEO::colorMode = RETRO_PIXEL_FORMAT_RGB565;
        if(!environ_cb(RETRO_ENVIRONMENT_SET_PIXEL_FORMAT, &VIDEO::colorMode))
        {
            VIDEO::colorMode = RETRO_PIXEL_FORMAT_0RGB1555;
        }
    }

    // Init DeSmuME
    struct NDS_fw_config_data fw_config;
    NDS_FillDefaultFirmwareConfigData(&fw_config);

    CommonSettings.num_cores = 2;
    CommonSettings.use_jit = true;

    addonsChangePak(NDS_ADDON_NONE);
    NDS_Init();
    NDS_CreateDummyFirmware(&fw_config);
    NDS_3D_ChangeCore(0);
    SPU_ChangeSoundCore(AUDIO::SNDCORE_RETRO, 735 * 2);
    backup_setManualBackupType(MC_TYPE_AUTODETECT);
}

void retro_deinit(void)
{
	NDS_DeInit();
}

void retro_reset (void)
{
    NDS_Reset();
}

void retro_run (void)
{
    poll_cb();

    // TOUCH
    const int16_t analogX = input_cb(0, RETRO_DEVICE_ANALOG, RETRO_DEVICE_INDEX_ANALOG_LEFT, RETRO_DEVICE_ID_ANALOG_X) / 2048;
    const int16_t analogY = input_cb(0, RETRO_DEVICE_ANALOG, RETRO_DEVICE_INDEX_ANALOG_LEFT, RETRO_DEVICE_ID_ANALOG_Y) / 2048;
    const bool r2Down =  input_cb(0, RETRO_DEVICE_JOYPAD, 0, RETRO_DEVICE_ID_JOYPAD_R2);    

    const bool haveMouse = (RETRO_DEVICE_MOUSE == INPUT::Devices[1]);
    const int16_t mouseX = haveMouse ? input_cb(1, RETRO_DEVICE_MOUSE, 0, RETRO_DEVICE_ID_MOUSE_X) : 0;
    const int16_t mouseY = haveMouse ? input_cb(1, RETRO_DEVICE_MOUSE, 0, RETRO_DEVICE_ID_MOUSE_Y) : 0;
    const bool mouseDown = haveMouse ? input_cb(1, RETRO_DEVICE_MOUSE, 0, RETRO_DEVICE_ID_MOUSE_LEFT) : false;
    
    INPUT::TouchX = INPUT::ClampedMove<0, 255>(INPUT::TouchX, mouseX + analogX);
    INPUT::TouchY = INPUT::ClampedMove<0, 191>(INPUT::TouchY, mouseY + analogY);
    
    if(r2Down || mouseDown)
    {
        NDS_setTouchPos(INPUT::TouchX, INPUT::TouchY);
    }
    else
    {
        NDS_releaseTouch();
    }

    // BUTTONS
    if(INPUT::Devices[0] == RETRO_DEVICE_JOYPAD)
    {
        NDS_beginProcessingInput();
        UserButtons& input = NDS_getProcessingUserInput().buttons;
        input.G = 0; // debug
        input.E = input_cb(0, RETRO_DEVICE_JOYPAD, 0, RETRO_DEVICE_ID_JOYPAD_R); // right shoulder
        input.W = input_cb(0, RETRO_DEVICE_JOYPAD, 0, RETRO_DEVICE_ID_JOYPAD_L); // left shoulder
        input.X = input_cb(0, RETRO_DEVICE_JOYPAD, 0, RETRO_DEVICE_ID_JOYPAD_X);
        input.Y = input_cb(0, RETRO_DEVICE_JOYPAD, 0, RETRO_DEVICE_ID_JOYPAD_Y);
        input.A = input_cb(0, RETRO_DEVICE_JOYPAD, 0, RETRO_DEVICE_ID_JOYPAD_A);
        input.B = input_cb(0, RETRO_DEVICE_JOYPAD, 0, RETRO_DEVICE_ID_JOYPAD_B);
        input.S = input_cb(0, RETRO_DEVICE_JOYPAD, 0, RETRO_DEVICE_ID_JOYPAD_START);
        input.T = input_cb(0, RETRO_DEVICE_JOYPAD, 0, RETRO_DEVICE_ID_JOYPAD_SELECT);
        input.U = input_cb(0, RETRO_DEVICE_JOYPAD, 0, RETRO_DEVICE_ID_JOYPAD_UP);
        input.D = input_cb(0, RETRO_DEVICE_JOYPAD, 0, RETRO_DEVICE_ID_JOYPAD_DOWN);
        input.L = input_cb(0, RETRO_DEVICE_JOYPAD, 0, RETRO_DEVICE_ID_JOYPAD_LEFT);
        input.R = input_cb(0, RETRO_DEVICE_JOYPAD, 0, RETRO_DEVICE_ID_JOYPAD_RIGHT);
        input.F = input_cb(0, RETRO_DEVICE_JOYPAD, 0, RETRO_DEVICE_ID_JOYPAD_L2); //Lid
        NDS_endProcessingInput();
    }

    // AUDIO: Reset frame count
    AUDIO::frames = 0;

    // RUN
    NDS_exec<false>();
    SPU_Emulate_user();
    
    // VIDEO: Swap screen colors and pass on
    if(RETRO_PIXEL_FORMAT_XRGB8888 == VIDEO::colorMode)
    {
        static uint32_t* const screenDest[2] = {(uint32_t*)&screenSwap[0], (uint32_t*)&screenSwap[256 * 192 * 4]};
        static const uint16_t* const screenSource[2] = {(uint16_t*)&GPU_screen[0], (uint16_t*)&GPU_screen[256 * 192 * 2]};
    
        VIDEO::SwapScreen32(screenDest[0], screenSource[0], 256);
        VIDEO::SwapScreen32(screenDest[1], screenSource[1], 256);
        INPUT::DrawPointer32(screenDest[1], 256);
        
        video_cb(screenSwap, 256, 192 * 2, 256 * 4);
    }
    else if(RETRO_PIXEL_FORMAT_RGB565 == VIDEO::colorMode)
    {
        static uint16_t* const screenDest[2] = {(uint16_t*)&screenSwap[0], (uint16_t*)&screenSwap[256 * 192 * 2]};
        static const uint16_t* const screenSource[2] = {(uint16_t*)&GPU_screen[0], (uint16_t*)&GPU_screen[256 * 192 * 2]};
    
        VIDEO::SwapScreen16<SWAPTYPE, 1>(screenDest[0], screenSource[0], 256);
        VIDEO::SwapScreen16<SWAPTYPE, 1>(screenDest[1], screenSource[1], 256);
        INPUT::DrawPointer16<0xFFFF>(screenDest[1], 256);
        
        video_cb(screenSwap, 256, 192 * 2, 256 * 2);
    }
    else if(RETRO_PIXEL_FORMAT_0RGB1555 == VIDEO::colorMode)
    {
        static uint16_t* const screenDest[2] = {(uint16_t*)&screenSwap[0], (uint16_t*)&screenSwap[256 * 192 * 2]};
        static const uint16_t* const screenSource[2] = {(uint16_t*)&GPU_screen[0], (uint16_t*)&GPU_screen[256 * 192 * 2]};
    
        VIDEO::SwapScreen16<SWAPTYPE, 0>(screenDest[0], screenSource[0], 256);
        VIDEO::SwapScreen16<SWAPTYPE, 0>(screenDest[1], screenSource[1], 256);
        INPUT::DrawPointer16<0x7FFF>(screenDest[1], 256);
        
        video_cb(screenSwap, 256, 192 * 2, 256 * 2);
    }
}

size_t retro_serialize_size (void)
{
    // HACK: Usually around 10 MB but can vary frame to frame!
    return 1024 * 1024 * 12;
}

bool retro_serialize(void *data, size_t size)
{
    EMUFILE_MEMORY state;
    savestate_save(&state, 0);
    
    if(state.size() <= size)
    {
        memcpy(data, state.buf(), state.size());
        return true;
    }
    
    return false;
}

bool retro_unserialize(const void * data, size_t size)
{
    EMUFILE_MEMORY state(const_cast<void*>(data), size);
    return savestate_load(&state);
}

void retro_cheat_reset(void)
{}

void retro_cheat_set(unsigned unused, bool unused1, const char* unused2)
{}

bool retro_load_game(const struct retro_game_info *game)
{
    execute = NDS_LoadROM(game->path);
    return execute;
}

bool retro_load_game_special(unsigned game_type, const struct retro_game_info *info, size_t num_info)
{
    if(RETRO_GAME_TYPE_SUPER_GAME_BOY == game_type && 2 == num_info)
    {
        strncpy(GBAgameName, info[1].path, sizeof(GBAgameName));
        addonsChangePak(NDS_ADDON_GBAGAME);
        
        return retro_load_game(&info[0]);
    }
    return false;
}

void retro_unload_game (void)
{
    NDS_FreeROM();
    execute = false;
}

unsigned retro_get_region (void)
{ 
   return RETRO_REGION_NTSC; 
}
