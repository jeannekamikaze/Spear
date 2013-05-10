#ifndef _SPEAR_IMAGE_ERROR_CODE_H
#define _SPEAR_IMAGE_ERROR_CODE_H

typedef enum
{
    Image_Success,
    Image_Read_Error,
    Image_Memory_Allocation_Error,
    Image_File_Not_Found,
    Image_Invalid_Format,
    Image_No_Suitable_Loader,
}
Image_error_code;

#endif // _SPEAR_IMAGE_ERROR_CODE_H
