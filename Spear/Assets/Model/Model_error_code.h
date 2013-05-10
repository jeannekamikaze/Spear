#ifndef _SPEAR_MODEL_ERROR_CODE_H
#define _SPEAR_MODEL_ERROR_CODE_H

typedef enum
{
    Model_Success,
    Model_Read_Error,
    Model_Memory_Allocation_Error,
    Model_File_Not_Found,
    Model_File_Mismatch,
    Model_No_Suitable_Loader,
}
Model_error_code;

#endif // _SPEAR_MODEL_ERROR_CODE_H

