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
    animation*  animations; // Holds the model's animations.
    
    unsigned int numFrames;
    unsigned int numVertices;   // Number of vertices per frame.
    unsigned int numTriangles;  // Number of triangles in one frame.
    unsigned int numTexCoords;  // Number of texture coordinates in one frame.
    unsigned int numSkins;
    unsigned int numAnimations;
}
Model;


typedef struct
{
    vec3 v0;
    vec3 v1;
    vec3 v2;
    vec3 n0;
    vec3 n1;
    vec3 n2;
    texCoord t0;
    texCoord t1;
    texCoord t2;
}
model_triangle;


#ifdef __cplusplus
extern "C" {
#endif

/// Frees the given Model from memory.
/// The 'model' pointer itself is not freed.
void model_free (Model* model);

/// Translate the Model such that its lowest point has y = 0.
void model_to_ground (Model* model);

/// Copy the triangles of the given frame from the Model into the given array.
void model_copy_triangles (Model* model, unsigned frame, model_triangle* tris);

/// Compute the model's 2d AABBs.
void model_compute_boxes (Model* model, float* points);

#ifdef __cplusplus
}
#endif

#endif // _SPEAR_MODEL_H
