#ifndef _BMP_LOAD_H
#define _BMP_LOAD_H


#include "../Image.h"
#include "../Image_error_code.h"


#ifdef __cplusplus
extern "C" {
#endif


/// Loads the BMP file specified by the given string.
/// (0,0) corresponds to the top left corner of the image.
Image_error_code BMP_load (const char* filename, Image* image);


#ifdef __cplusplus
}
#endif

#endif // _BMP_LOAD_H
