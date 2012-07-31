#include "Image.h"
#include <stdlib.h>


void image_free (Image* image)
{
    free (image->pixels);
} 
