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


void model_transform (Model* model, float mat[16], float normal[9])
{
    unsigned i = 0;
    unsigned j = model->numVertices * model->numFrames;
    vec3* v = model->vertices;
    vec3* n = model->normals;
    
    for (; i < j; ++i)
    {
        mul (mat, v);
        mul_normal (normal, n);
        v++;
        n++;
    }
}
