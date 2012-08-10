#include "Model.h"
#include <stdlib.h> // free
#include <math.h>


#define TO_RAD M_PI / 180.0


static void safe_free (void* ptr)
{
    if (ptr)
    {
        free (ptr);
        ptr = 0;
    }
}


void model_free (Model* model)
{
    safe_free (model->vertices);
    safe_free (model->normals);
    safe_free (model->texCoords);
    safe_free (model->triangles);
    safe_free (model->skins);
    safe_free (model->animations);
}


static void mul (float m[16], vec3* v)
{
    float x = v->x;
    float y = v->y;
    float z = v->z;
    v->x = x*m[0] + y*m[4] + z*m[8]  + m[12];
    v->y = x*m[1] + y*m[5] + z*m[9]  + m[13];
    v->z = x*m[2] + y*m[6] + z*m[10] + m[14];
}


static void mul_normal (float m[9], vec3* n)
{
    float x = n->x;
    float y = n->y;
    float z = n->z;
    n->x = x*m[0] + y*m[3] + z*m[6];
    n->y = x*m[1] + y*m[4] + z*m[7];
    n->z = x*m[2] + y*m[5] + z*m[8];
    x = n->x;
    y = n->y;
    z = n->z;
    float mag = sqrt(x*x + y*y + z*z);
    mag = mag == 0.0 ? 1.0 : mag;
    n->x /= mag;
    n->y /= mag;
    n->z /= mag;
}


void model_transform_vertices (Model* model, float mat[16])
{
    unsigned i = 0;
    unsigned j = model->numVertices * model->numFrames;
    vec3* v = model->vertices;
    
    for (; i < j; ++i, ++v)
    {
        mul (mat, v);
    }
}


void model_transform_normals (Model* model, float normal[9])
{
    unsigned i = 0;
    unsigned j = model->numVertices * model->numFrames;
    vec3* n = model->normals;
    
    for (; i < j; ++i, ++n)
    {
        mul_normal (normal, n);
    }
}


void model_to_ground (Model* model)
{
    unsigned i, f;
    vec3* v = model->vertices;
    
    // Compute the minimum y coordinate for each frame and translate
    // the model appropriately.
    for (f = 0; f < model->numFrames; ++f)
    {
        vec3* w = v;
        float y = v->y;
        
        for (i = 0; i < model->numVertices; ++i, ++v)
        {
            y = fmin (y, v->y);
        }
        
        v = w;
        for (i = 0; i < model->numVertices; ++i, ++v)
        {
            v->y -= y;
        }
    }
}


void model_copy_triangles (Model* model, unsigned frame, model_triangle* tris)
{
    int i;
    int j = model->numVertices;
    
    vec3* v = model->vertices + j * frame;
    vec3* n = model->normals  + j * frame;
    texCoord* t = model->texCoords;
    triangle* tri = model->triangles;
    
    
    for (i = 0; i < j; ++i, ++tri, ++tris)
    {
        tris->v0 = v[tri->vertexIndices[0]];
        tris->v1 = v[tri->vertexIndices[1]];
        tris->v2 = v[tri->vertexIndices[2]];
        
        tris->n0 = n[tri->vertexIndices[0]];
        tris->n1 = n[tri->vertexIndices[1]];
        tris->n2 = n[tri->vertexIndices[2]];
        
        tris->t0 = t[tri->textureIndices[0]];
        tris->t1 = t[tri->textureIndices[1]];
        tris->t2 = t[tri->textureIndices[2]];
    }
}
