#ifndef _SPEAR_MODEL_H
#define _SPEAR_MODEL_H

#include "sys_types.h"


typedef struct
{
    char name[64];
}
skin;


typedef struct
{
    float x, y, z;
}
vec3;


typedef struct
{
    float s, t;
}
texCoord;


typedef struct
{
    U16 vertexIndices[3];
    U16 textureIndices[3];
}
triangle;


typedef struct
{
    char name[16];
    unsigned int start;
    unsigned int end;
}
animation;


typedef struct
{
    vec3*       vertices;   // One array per frame.
    vec3*       normals;    // One array per frame. One normal per vertex per frame.
    texCoord*   texCoords;  // One array for all frames.
    triangle*   triangles;  // One array for all frames.
    skin*       skins;      // Holds the model's texture files.
    animation* animations; // Holds the model's animations.
    
    unsigned int numFrames;
    unsigned int numVertices;   // Number of vertices per frame.
    unsigned int numTriangles;  // Number of triangles in one frame.
    unsigned int numTexCoords;  // Number of texture coordinates in one frame.
    unsigned int numSkins;
    unsigned int numAnimations;
}
Model;


#ifdef __cplusplus
extern "C" {
#endif

/// Frees the given Model from memory.
/// The 'model' pointer itself is not freed.
void model_free (Model* model);

/// Transform the Model's vertices by the given matrix.
void model_transform_verts (Model* model, float mat[16]);

/// Transform the Model's normals by the given matrix.
void model_transform_normals (Model* model, float normal[9]);

/// Translate the Model such that its lowest point has y = 0.
void model_to_ground (Model* model);

#ifdef __cplusplus
}
#endif

#endif // _SPEAR_MODEL_H
