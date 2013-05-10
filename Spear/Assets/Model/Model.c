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


void model_compute_boxes (Model* model, float* points)
{
    vec3* v = model->vertices;
    
    unsigned f;
    for (f = 0; f < model->numFrames; ++f)
    {
        float xmin = v->x;
        float xmax = v->x;
        float ymin = v->y;
        float ymax = v->y;
        float zmin = v->z;
        float zmax = v->z;
        
        unsigned i;
        for (i = 0; i < model->numVertices; ++i, ++v)
        {
            xmin = fmin (xmin, v->x);
            ymin = fmin (ymin, v->y);
            zmin = fmin (zmin, v->z);
            xmax = fmax (xmax, v->x);
            ymax = fmax (ymax, v->y);
            zmax = fmax (zmax, v->z);
        }
        
        *points++ = xmin; *points++ = ymin; *points++ = zmin;
        *points++ = xmax; *points++ = ymax; *points++ = zmax;
    }
}
