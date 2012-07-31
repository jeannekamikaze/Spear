#include "RenderModel.h"
#include <stdlib.h> // free
#include <string.h> // memcpy
#include <stdio.h>


static void safe_free (void* ptr)
{
    if (ptr)
    {
        free (ptr);
        ptr = 0;
    }
}


/// Populate elements of an animated model to be rendered from
/// start to end in a loop.
/*int populate_elements_animated (Model* model_asset, RenderModel* model)
{
    size_t nverts     = model_asset->numVertices;
    size_t ntriangles = model_asset->numTriangles;
    size_t nframes    = model_asset->numFrames;
    size_t n          = nframes * ntriangles * 3;
    
    model->elements = malloc (56 * n);
    if (!model->elements) return -1;
    
    // Populate elements.
    
    size_t f, i;
    
    char* elem      = (char*) model->elements;
    vec3* v1        = model_asset->vertices;
    vec3* v2        = v1 + nverts;
    vec3* n1        = model_asset->normals;
    vec3* n2        = n1 + nverts;
    texCoord* tex   = model_asset->texCoords;
    
    for (f = 0; f < nframes; ++f)
    {
        triangle* t = model_asset->triangles;
        
        for (i = 0; i < ntriangles; ++i)
        {
            *((vec3*)     elem)        = v1[t->vertexIndices[0]];
            *((vec3*)     (elem + 12)) = v2[t->vertexIndices[0]];
            *((vec3*)     (elem + 24)) = n1[t->vertexIndices[0]];
            *((vec3*)     (elem + 36)) = n2[t->vertexIndices[0]];
            *((texCoord*) (elem + 48)) = tex[t->textureIndices[0]];
            elem += 56;
            
            *((vec3*)     elem)        = v1[t->vertexIndices[1]];
            *((vec3*)     (elem + 12)) = v2[t->vertexIndices[1]];
            *((vec3*)     (elem + 24)) = n1[t->vertexIndices[1]];
            *((vec3*)     (elem + 36)) = n2[t->vertexIndices[1]];
            *((texCoord*) (elem + 48)) = tex[t->textureIndices[1]];
            elem += 56;
            
            *((vec3*)     elem)        = v1[t->vertexIndices[2]];
            *((vec3*)     (elem + 12)) = v2[t->vertexIndices[2]];
            *((vec3*)     (elem + 24)) = n1[t->vertexIndices[2]];
            *((vec3*)     (elem + 36)) = n2[t->vertexIndices[2]];
            *((texCoord*) (elem + 48)) = tex[t->textureIndices[2]];
            elem += 56;
            
            t++;
        }
        
        v1 += nverts;
        v2 += nverts;
        n1 += nverts;
        n2 += nverts;
        
        if (f == nframes-2)
        {
            v2 = model_asset->vertices;
            n2 = model_asset->normals;
        }
    }
    
    return 0;
}*/


/// Populate elements of an animated model according to its frames
/// of animation.
int populate_elements_animated (Model* model_asset, RenderModel* model)
{
    size_t nverts     = model_asset->numVertices;
    size_t ntriangles = model_asset->numTriangles;
    size_t nframes    = model_asset->numFrames;
    size_t n          = nframes * ntriangles * 3;
    
    model->elements = malloc (56 * n);
    if (!model->elements) return -1;
    
    // Populate elements.
    
    unsigned f, i, j, u;
    
    char* elem      = (char*) model->elements;
    animation* anim  = model_asset->animations;
    
    for (i = 0; i < model_asset->numAnimations; ++i, anim++)
    {
        unsigned start = anim->start;
        unsigned end   = anim->end;
        
        char singleFrameAnim = start == end;
        
        vec3* v1      = model_asset->vertices + start*nverts;
        vec3* v2      = singleFrameAnim ? v1 : v1 + nverts;
        vec3* n1      = model_asset->normals + start*nverts;
        vec3* n2      = singleFrameAnim ? n1 : n1 + nverts;
        texCoord* tex = model_asset->texCoords;
        
        for (u = start; u <= end; ++u)
        {
            triangle* t = model_asset->triangles;
            
            for (j = 0; j < ntriangles; ++j, t++)
            {
                *((vec3*)     elem)        = v1[t->vertexIndices[0]];
                *((vec3*)     (elem + 12)) = v2[t->vertexIndices[0]];
                *((vec3*)     (elem + 24)) = n1[t->vertexIndices[0]];
                *((vec3*)     (elem + 36)) = n2[t->vertexIndices[0]];
                *((texCoord*) (elem + 48)) = tex[t->textureIndices[0]];
                elem += 56;
                
                *((vec3*)     elem)        = v1[t->vertexIndices[1]];
                *((vec3*)     (elem + 12)) = v2[t->vertexIndices[1]];
                *((vec3*)     (elem + 24)) = n1[t->vertexIndices[1]];
                *((vec3*)     (elem + 36)) = n2[t->vertexIndices[1]];
                *((texCoord*) (elem + 48)) = tex[t->textureIndices[1]];
                elem += 56;
                
                *((vec3*)     elem)        = v1[t->vertexIndices[2]];
                *((vec3*)     (elem + 12)) = v2[t->vertexIndices[2]];
                *((vec3*)     (elem + 24)) = n1[t->vertexIndices[2]];
                *((vec3*)     (elem + 36)) = n2[t->vertexIndices[2]];
                *((texCoord*) (elem + 48)) = tex[t->textureIndices[2]];
                elem += 56;
            }
            
            // Advance to the next frame of animation of the current
            // animation.
            v1 += nverts;
            v2 += nverts;
            n1 += nverts;
            n2 += nverts;
            
            // Reset the secondary pointers to the beginning of the
            // animation when we are about to reach the last frame.
            if (u == end-1)
            {
                v2 = model_asset->vertices + start*nverts;
                n2 = model_asset->normals  + start*nverts;
            }
        }
    }
    
    return 0;
}


int populate_elements_static (Model* model_asset, RenderModel* model)
{
    size_t nverts     = model_asset->numVertices;
    size_t ntriangles = model_asset->numTriangles;
    size_t n          = ntriangles * 3;
    
    model->elements = malloc (32 * n);
    if (!model->elements) return -1;
    
    // Populate elements.
    
    size_t f, i;
    
    char* elem     = (char*) model->elements;
    vec3* vert     = model_asset->vertices;
    vec3* norm     = model_asset->normals;
    texCoord* tex  = model_asset->texCoords;
    
    triangle* t = model_asset->triangles;
    
    for (i = 0; i < ntriangles; ++i)
    {
        *((vec3*)     elem)        = vert[t->vertexIndices[0]];
        *((vec3*)     (elem + 12)) = norm[t->vertexIndices[0]];
        *((texCoord*) (elem + 24)) = tex[t->textureIndices[0]];
        elem += 32;
        
        *((vec3*)     elem)        = vert[t->vertexIndices[1]];
        *((vec3*)     (elem + 12)) = norm[t->vertexIndices[1]];
        *((texCoord*) (elem + 24)) = tex[t->textureIndices[1]];
        elem += 32;
        
        *((vec3*)     elem)        = vert[t->vertexIndices[2]];
        *((vec3*)     (elem + 12)) = norm[t->vertexIndices[2]];
         *((texCoord*) (elem + 24)) = tex[t->textureIndices[2]];
        elem += 32;
        
        t++;
    }
    
    return 0;
}


int render_model_from_model_asset (Model* model_asset, RenderModel* model)
{
    U32 ntriangles = model_asset->numTriangles;
    U32 nframes    = model_asset->numFrames;
    
    int result;
    if (nframes > 1) result = populate_elements_animated (model_asset, model);
    else             result = populate_elements_static   (model_asset, model);
    
    if (result != 0) return result;
    
    model->numFrames   = nframes;
    model->numVertices = ntriangles * 3; // Number of vertices per frame.
    
    return 0;
}


void render_model_free (RenderModel* model)
{
    safe_free (model->elements);
}
