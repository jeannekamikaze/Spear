#include "BMP_load.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#define BITMAP_ID   0x4D42


/// Bitmap file header structure.
typedef struct
{
    U16 type;           // Specifies the image type; must be BM (0x4D42).
    U32 size;           // Specifies the size in bytes of the bitmap file.
    U32 reserved;       // Reserved; must be zero.
    U32 offBits;        // Specifies the offset, in bytes, from the BitmapFileHeader structure to the bitmap bits.
}
BitmapFileHeader;


/// Bitmap info header structure.
typedef struct
{
    U32 size;           // Specifies the number of bytes required by the structure.
    U32 width;          // Specifies the width of the bitmap, in pixels.
    U32 height;         // Specifies the height of the bitmap, in pixels.
    U16 planes;         // Specifies the number of color planes; must be 1.
    U16 bitCount;       // Specifies the number of bits per pixel; must be 1, 4, 16, 24, or 32.
    U32 compression;    // Specifies the type of compression.
    U32 imageSize;      // Specifies the size of the image in bytes.
    U32 xPelsPerMeter;  // Specifies the number of pixels per meter on the x axis.
    U32 yPelsPerMeter;  // Specifies the number of pixels per meter on the y axis.
    U32 clrUsed;        // Specifies the number of colours used by the bitmap.
    U32 clrImportant;   // Specifies the number of colours that are important.
}
BitmapInfoHeader;


static void safe_free (void* ptr)
{
    if (ptr) free (ptr);
}


static Image_error_code read_raw_data(
    FILE* filePtr, const BitmapFileHeader* bitmapFileHeader,
    const BitmapInfoHeader* bitmapInfoHeader, U8** data)
{
    U8* bitmapImage;
    U8* auxrow;
    size_t row_size  = bitmapInfoHeader->width * 3;
    size_t numBytes = bitmapInfoHeader->height * row_size;
    size_t bytes_read;
    
    // Allocate memory for the bitmap data and the auxiliary row.
    bitmapImage = (U8*) malloc (numBytes);
    auxrow = (U8*) malloc (row_size);
    if (!bitmapImage || !auxrow)
    {
        safe_free (bitmapImage);
        safe_free (auxrow);
        return Image_Memory_Allocation_Error;
    }
    
    // Move the file pointer to the beginning of bitmap data and read the data.
    fseek(filePtr, bitmapFileHeader->offBits, SEEK_SET);
    bytes_read = fread(bitmapImage, 1, numBytes, filePtr);
    if (bytes_read != numBytes)
    {
        free(bitmapImage);
        return Image_Read_Error;
    }
    
    size_t i;
    
    // Reverse rows.
    /*size_t h = bitmapInfoHeader->height / 2;
    for (i = 0; i < h; ++i)
    {
        U8* row1 = bitmapImage + i * row_size;
        U8* row2 = bitmapImage + (bitmapInfoHeader->height - i - 1) * row_size;
        
        memcpy (auxrow, row1, row_size);
        memcpy (row1, row2, row_size);
        memcpy (row2, auxrow, row_size);
    }*/
    
    // Swap B and R channels. BGR -> RGB.
    for (i = 0; i < bytes_read; i+= 3)
    {
        U8 tmp = bitmapImage[i];
        bitmapImage[i] = bitmapImage[i+2];
        bitmapImage[i+2] = tmp;
    }
    
    *data = bitmapImage;

    return Image_Success;
}


static Image_error_code read_paletised_data8
    (FILE* filePtr, const BitmapInfoHeader *bitmapInfoHeader, U8** data)
{
    U8* bitmapImage;
    U8* palette;
    U8* colourIndices;
    size_t bytes_read;
    
    size_t paletteSize = pow(2, bitmapInfoHeader->bitCount) * 4;
    size_t colourIndicesSize = bitmapInfoHeader->width * bitmapInfoHeader->height;
    int bitmapImageSize = colourIndicesSize * 3;
    
    // Save memory for the palette, colour indices and bitmap image.
    palette = (U8*) malloc (paletteSize);
    colourIndices = (U8*) malloc(colourIndicesSize);
    bitmapImage = (U8*) malloc(bitmapImageSize);
    if (!palette | !colourIndices || !bitmapImage)
    {
        safe_free (palette);
        safe_free (colourIndices);
        safe_free (bitmapImage);
        return Image_Memory_Allocation_Error;
    }
    
    // Read the colour palette.
    bytes_read = fread(palette, 1, paletteSize, filePtr);
    if (bytes_read != paletteSize) return Image_Read_Error;
    
    // Read the colour indices.
    bytes_read = fread(colourIndices, 1, colourIndicesSize, filePtr);
    if (bytes_read != colourIndicesSize) return Image_Read_Error;
    
    // Decode the image data.
    U8* imgptr = &bitmapImage[bitmapImageSize - (bitmapInfoHeader->width * 4)];
    
    size_t i;
    for (i = 0;  i < colourIndicesSize; i++)
    {
        int index = colourIndices[i];
        
        memcpy(imgptr, (const void*) &palette[index * 4], 3);
        imgptr += 3;
        
        if (!((i+1) % bitmapInfoHeader->width))
        {
            imgptr -= (bitmapInfoHeader->width * 4 * 2);
        }
    }
    
    free(palette);
    free(colourIndices);
    
    *data = bitmapImage;
    
    return Image_Success;
}


Image_error_code BMP_load (const char* filename, Image* image)
{
    FILE* filePtr;
    BitmapFileHeader bitmapFileHeader;
    BitmapInfoHeader bitmapInfoHeader;
    U8 buf[40];
    U8* data;
    
    // Open the file in binary read-only mode.
    filePtr = fopen(filename, "rb");
    if (!filePtr) return Image_File_Not_Found;
    
    if ((fread(buf, 14, 1, filePtr)) != 1)
    {
        fclose(filePtr);
        return Image_Read_Error;
    }

    bitmapFileHeader.type       = *((U16*)buf);
    bitmapFileHeader.size       = *((U32*)(buf+2));
    bitmapFileHeader.reserved   = *((U32*)(buf+6));
    bitmapFileHeader.offBits    = *((U32*)(buf+10));
    
    // Check that this is in fact a BMP file.
    if (bitmapFileHeader.type != BITMAP_ID)
    {
        fprintf(stderr, "Not a valid BMP file\n");
        fclose(filePtr);
        return Image_Invalid_Format;
    }
    
    if ((fread(&buf, 40, 1, filePtr)) != 1)
    {
        fclose(filePtr);
        return Image_Read_Error;
    }
    
    bitmapInfoHeader.size           = *((U32*)(buf));
    bitmapInfoHeader.width          = *((U32*)(buf+4));
    bitmapInfoHeader.height         = *((U32*)(buf+8));
    bitmapInfoHeader.planes         = *((U16*)(buf+12));
    bitmapInfoHeader.bitCount       = *((U16*)(buf+14));
    bitmapInfoHeader.compression    = *((U32*)(buf+16));
    bitmapInfoHeader.imageSize      = *((U32*)(buf+20));
    bitmapInfoHeader.xPelsPerMeter  = *((U32*)(buf+24));
    bitmapInfoHeader.yPelsPerMeter  = *((U32*)(buf+28));
    bitmapInfoHeader.clrUsed        = *((U32*)(buf+32));
    bitmapInfoHeader.clrImportant   = *((U32*)(buf+36));
    
    // Check that no compression is used.
    // Compression is not supported at the moment.
    if (bitmapInfoHeader.compression != 0)
    {
        fprintf(stderr, "Compression not supported\n");
        fclose(filePtr);
        return Image_Invalid_Format;
    }
    
    // Check that this is a Windows BMP file.
    // Other formats are not supported.
    if (bitmapInfoHeader.size != 40)
    {
        fprintf(stderr, "Only Windows BMP files supported\n");
        fclose(filePtr);
        return Image_Invalid_Format;
    }
    
    Image_error_code status;
    
    if (bitmapInfoHeader.bitCount == 8)
    {
        // The BMP file uses a colour palette.
        // We are already positioned at the colour palette.
        status = read_paletised_data8 (filePtr, &bitmapInfoHeader, &data);
    }
    else if (bitmapInfoHeader.bitCount >= 16)
    {
        // The BMP file uses no colour palette.
        status = read_raw_data (filePtr, &bitmapFileHeader, &bitmapInfoHeader, &data);
    }
    else
    {
        fprintf(stderr, "Only 24-bit and 16-bit palette images supported\n");
        fclose(filePtr);
        return Image_Invalid_Format;
    }
    
    fclose(filePtr);
    
    if (data == 0) return status;
    
    image->width  = bitmapInfoHeader.width;
    image->height = bitmapInfoHeader.height;
    image->bpp    = 3;
    image->pixels = data;
    
    return Image_Success;
}
