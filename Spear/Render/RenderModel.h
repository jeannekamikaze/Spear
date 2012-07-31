#ifndef _SPEAR_RENDER_MODEL_H
#define _SPEAR_RENDER_MODEL_H

#include "Model.h"


/// Represents a renderable model.
/**
 * If the model is animated:
 * 
 *      Buffer layout:
 *          vert1 vert2 norm1 norm2 texc
 *          
 *          element size = (3 + 3 + 3 + 3 + 2)*4 = 56 B
 *          buffer  size = element size * num vertices = 56n
 * 
 * If the model is static:
 * 
 *      Buffer layout:
 *          vert norm texc
 *          
 *          element size = (3 + 3 + 2)*4 = 32 B
 *          buffer  size = element size * num vertices = 32n
 * 
 **/
typedef struct
{
    void*  elements;
    U32    numFrames;
    U32    numVertices; // Number of vertices per frame.
}
RenderModel;


#ifdef __cplusplus
extern "C" {
#endif

int render_model_from_model_asset (Model* model_asset, RenderModel* render_model);

void render_model_free (RenderModel* model);

#ifdef __cplusplus
}
#endif


#endif // _SPEAR_RENDER_MODEL_H

