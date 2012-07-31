#ifndef _MD2_LOAD_H
#define _MD2_LOAD_H

#include "../Model.h"
#include "../Model_error_code.h"


#ifdef __cplusplus
extern "C" {
#endif

/// Loads the MD2 file specified by the given string.
/// 'clockwise' should be 1 if you plan to render the model in a clockwise environment, 0 otherwise.
/// 'smooth_normals' should be 1 if you want the loader to compute smooth normals, 0 otherwise.
Model_error_code MD2_load (const char* filename, char clockwise, char left_handed, Model* model);

#ifdef __cplusplus
}
#endif


#endif // _MD2_LOAD_H

