#include "OBJ_load.h"
#include "cvector.h"
#include <stdio.h>
#include <stdlib.h> // free
#include <string.h> // memcpy
#include <math.h> // sqrt


char lastError [128];


static void safe_free (void* ptr)
{
    if (ptr)
    {
        free (ptr);
        ptr = 0;
    }
}


static void cross (vec3 a, vec3 b, vec3* c)
{
    c->x = a.y * b.z - a.z * b.y;
    c->y = a.z * b.x - a.x * b.z;
    c->z = a.x * b.y - a.y * b.x;
}


static void vec3_sub (vec3 a, vec3 b, vec3* out)
{
    out->x = a.x - b.x;
    out->y = a.y - b.y;
    out->z = a.z - b.z;
}


static void compute_normal (char clockwise, vec3 p1, vec3 p2, vec3 p3, vec3* n)
{
    vec3 v1, v2;
    if (!clockwise)
    {
        vec3_sub (p3, p2, &v1);
        vec3_sub (p1, p2, &v2);
    }
    else
    {
        vec3_sub (p1, p2, &v1);
        vec3_sub (p3, p2, &v2);
    }
    cross (v1, v2, n);
}


static void normalise (vec3* v)
{
    float x = v->x;
    float y = v->y;
    float z = v->z;
    float mag = sqrt (x*x + y*y + z*z);
    mag = mag == 0.0f ? 1.0f : mag;
    v->x /= mag;
    v->y /= mag;
    v->z /= mag;
}


static void vec3_add (vec3 a, vec3* b)
{
    b->x += a.x;
    b->y += a.y;
    b->z += a.z;
}


static void read_vertex (FILE* file, vec3* vert)
{
    fscanf (file, "%f %f", &vert->x, &vert->y);
    if (fscanf(file, "%f", &vert->z) == 0) vert->z = 0.0f;
}


static void read_normal (FILE* file, vec3* normal)
{
    fscanf (file, "%f %f %f", &normal->x, &normal->y, &normal->z);
}


static void read_tex_coord (FILE* file, texCoord* texc)
{
    fscanf (file, "%f %f", &texc->s, &texc->t);
}


static void read_face (FILE* file,
                       char clockwise,
                       vector* vertices,
                       vector* normals,
                       vector* triangles)
{
    vector idxs;
    vector texCoords;
    
    vector_new (&idxs, sizeof(int), 4);
    vector_new (&texCoords, sizeof(int), 4);
    
    unsigned int index;
    unsigned int normal;
    unsigned int texc;
    
    fscanf (file, "f");
    
    while (!feof(file) && fscanf(file, "%d", &index) > 0)
    {
        vector_append (&idxs, &index);
        
        if (fgetc (file) == '/')
        {
            fscanf (file, "%d", &texc);
            vector_append (&texCoords, &texc);
        }
        else fseek (file, -1, SEEK_CUR);
        
        if (fgetc (file) == '/')
        {
            fscanf (file, "%d", &normal);
        }
        else fseek (file, -1, SEEK_CUR);
    }
    
    // Triangulate the face and add its triangles to the triangles vector.
    triangle tri;
    tri.vertexIndices[0]  = *((int*) vector_ith (&idxs, 0)) - 1;
    tri.textureIndices[0] = *((int*) vector_ith (&texCoords, 0)) - 1;
    
    int i;
    for (i = 1; i < vector_size(&idxs)-1; i++)
    {
        tri.vertexIndices[1]  = *((int*) vector_ith (&idxs, i)) - 1;
        tri.textureIndices[1] = *((int*) vector_ith (&texCoords, i)) - 1;
        tri.vertexIndices[2]  = *((int*) vector_ith (&idxs, i+1)) - 1;
        tri.textureIndices[2] = *((int*) vector_ith (&texCoords, i+1)) - 1;
        vector_append (triangles, &tri);
    }
    
    // Compute face normal and add contribution to each of the face's vertices.
    unsigned int i0 = tri.vertexIndices[0];
    unsigned int i1 = tri.vertexIndices[1];
    unsigned int i2 = tri.vertexIndices[2];
    
    vec3 n;
    vec3 v0 = *((vec3*) vector_ith (vertices, i0));
    vec3 v1 = *((vec3*) vector_ith (vertices, i1));
    vec3 v2 = *((vec3*) vector_ith (vertices, i2));
    compute_normal (clockwise, v0, v1, v2, &n);
    
    for (i = 0; i < vector_size (&idxs); i++)
    {
        int j = *((int*) vector_ith (&idxs, i)) - 1;
        vec3* normal = (vec3*) vector_ith (normals, j);
        vec3_add (n, normal);
    }
    
    vector_free (&idxs);
    vector_free (&texCoords);
}


Model_error_code OBJ_load (const char* filename, char clockwise, char left_handed, Model* model)
{
    vec3* norms     = 0;
    vec3* verts     = 0;
    texCoord* texcs = 0;
    triangle* tris  = 0;
    
    FILE* file = fopen (filename, "r");
    if (file == NULL) return Model_File_Not_Found;
    
    vec3 vert;
    vec3 normal;
    texCoord texc;
    
    vector vertices;
    vector normals;
    vector texCoords;
    vector triangles;
    
    int result = vector_new (&vertices,  sizeof(vec3),     0)
               | vector_new (&normals,   sizeof(vec3),     0)
               | vector_new (&texCoords, sizeof(texCoord), 0)
               | vector_new (&triangles, sizeof(triangle), 0);
    
    if (result != 0)
    {
        safe_free (vertices.data);
        safe_free (normals.data);
        safe_free (texCoords.data);
        safe_free (triangles.data);
        return Model_Memory_Allocation_Error;
    }
    
    while (!feof(file))
    {
        switch (fgetc(file))
        {
        case 'v':
            switch (fgetc(file))
            {
            case 't':
                read_tex_coord (file, &texc);
                vector_append (&texCoords, &texc);
                break;
                
            case 'n':
                read_normal (file, &normal);
                vector_append (&normals, &normal);
                break;
                
            default:
                read_vertex (file, &vert);
                vector_append (&vertices, &vert);
                break;
            }
            break;
            
        case 'f':
            // Initialise the normals vector if it is empty.
            if (vector_size(&normals) == 0)
            {
                vec3 zero;
                zero.x = 0.0f; zero.y = 0.0f; zero.z = 0.0f;
                vector_new (&normals, sizeof(vec3), vector_size(&vertices));
                vector_initialise (&normals, &zero);
            }
            read_face (file, clockwise, &vertices, &normals, &triangles);
            break;
        
        case '#':
        {
            int x = 17;
            while (x != '\n' && x != EOF) x = fgetc(file);
            break;
        }
            
        default: break;
        }
    }
    
    fclose (file);
    
    unsigned numVertices = vector_size (&vertices);
    
    // Normalise normals.
    unsigned i;
    for (i = 0; i < numVertices; ++i)
    {
        normalise (vector_ith (&normals, i));
    }
    
    model->vertices      = (vec3*) vertices.data;
    model->normals       = (vec3*) normals.data;
    model->texCoords     = (texCoord*) texCoords.data;
    model->triangles     = (triangle*) triangles.data;
    model->skins         = 0;
    model->animations    = 0;
    model->numFrames     = 1;
    model->numVertices   = numVertices;
    model->numTriangles  = vector_size (&triangles);
    model->numTexCoords  = vector_size (&texCoords);
    model->numSkins      = 0;
    model->numAnimations = 0;
    
    return Model_Success;
}
