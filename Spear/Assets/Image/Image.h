#ifndef _SPEAR_IMAGE_H
#define _SPEAR_IMAGE_H

#include "sys_types.h"


typedef struct
{
    int width;
    int height;
    int bpp;    // Bits per pixel.
                // If bpp = 3 then format = RGB.
                // If bpp = 4 then format = RGBA.
    U8* pixels;
}
Image;


#ifdef __cplusplus
extern "C" {
#endif

/// Frees the given Image from memory.
/// The 'image' pointer itself is not freed.
void image_free (Image* image);

#ifdef __cplusplus
}
#endif


#endif // _SPEAR_IMAGE_H
