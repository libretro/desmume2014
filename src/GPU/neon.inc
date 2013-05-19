#include <arm_neon.h>

// This function applies the darken effect to eight pixels
static FORCEINLINE void NEONdarkenColor(uint16x8_t* color, uint32_t factor)
{
   static const uint16x8_t mask = vmovq_n_u16(0x1F);

   uint16x8_t r = *color;
   uint16x8_t g = vshrq_n_u16(r,  5);
   uint16x8_t b = vshrq_n_u16(r, 10);

   uint16x8_t r2 = vandq_u16(r, mask);
   uint16x8_t g2 = vandq_u16(g, mask);
   uint16x8_t b2 = vandq_u16(b, mask);

   r = vmulq_n_u16(r2, factor);
   g = vmulq_n_u16(g2, factor);
   b = vmulq_n_u16(b2, factor);

   r = vshrq_n_u16(r, 4);
   g = vshrq_n_u16(g, 4);
   b = vshrq_n_u16(b, 4);

   r2 = vsubq_u16(r2, r);
   g2 = vsubq_u16(g2, g);
   b2 = vsubq_u16(b2, b);

   *color = r2;
   g = vshlq_n_u16(g2,  5);
   b = vshlq_n_u16(b2, 10);

   *color = vorrq_u16(*color, g);
   *color = vorrq_u16(*color, b);
}
