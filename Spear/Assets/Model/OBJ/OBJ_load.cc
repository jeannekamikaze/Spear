#include "OBJ_load.h"
#include <cstdio>
#include <cstdlib> // free
#include <cstring> // memcpy
#include <cmath> // sqrt
#include <vector>


char lastError [128];


static void safe_free (void* ptr)
{
    if (ptr)
    {
        free (ptr);
        ptr = 0;
    }
}


// Cross product.
// (0,1,0) x (1,0,0) = (0,0,-1).
static void cross (const vec3& a, const vec3& b, vec3& c)
{
    c.x = a.y * b.z - a.z * b.y;
    c.y = a.z * b.x - a.x * b.z;
    c.z = a.x * b.y - a.y * b.x;
}


static void vec3_sub (const vec3& a, const vec3& b, vec3& out)
{
    out.x = a.x - b.x;
    out.y = a.y - b.y;
    out.z = a.z - b.z;
}


static void compute_normal (char clockwise, const vec3& p1, const vec3& p2, const vec3& p3, vec3& n)
{
    vec3 v1, v2;
    if (clockwise)
    {
        vec3_sub (p3, p2, v1);
        vec3_sub (p1, p2, v2);
    }
    else
    {
        vec3_sub (p1, p2, v1);
        vec3_sub (p3, p2, v2);
    }
    cross (v1, v2, n);
}


static void normalise (vec3& v)
{
    float x = v.x;
    float y = v.y;
    float z = v.z;
    float mag = sqrt (x*x + y*y + z*z);
    mag = mag == 0.0f ? 1.0f : mag;
    v.x /= mag;
    v.y /= mag;
    v.z /= mag;
}


static void vec3_add (const vec3& a, vec3& b)
{
    b.x += a.x;
    b.y += a.y;
    b.z += a.z;
}


static void read_vertex (FILE* file, vec3& vert)
{
    fscanf (file, "%f %f", &vert.x, &vert.y);
    if (fscanf(file, "%f", &vert.z) == 0) vert.z = 0.0f;
}


static void read_normal (FILE* file, vec3& normal)
{
    fscanf (file, "%f %f %f", &normal.x, &normal.y, &normal.z);
}


static void read_tex_coord (FILE* file, texCoord& texc)
{
    fscanf (file, "%f %f", &texc.s, &texc.t);
}


static void read_face (FILE* file, char clockwise,
                       const std::vector<vec3>& vertices,
                       std::vector<vec3>& normals,
                       std::vector<triangle>& triangles)
{
    std::vector<unsigned int> idxs;
    std::vector<unsigned int> texCoords;
    
    unsigned int index;
    unsigned int normal;
    unsigned int texc;
    
    fscanf (file, "f");
    
    while (!feof(file) && fscanf(file, "%d", &index) > 0)
    {
        idxs.push_back(index);
        
        if (fgetc (file) == '/')
        {
            fscanf (file, "%d", &texc);
            texCoords.push_back(texc);
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
    tri.vertexIndices[0] = idxs[0] - 1;
    tri.textureIndices[0] = texCoords[0] - 1;
    
    for (int i = 1; i < idxs.size()-1; i++)
    {
        tri.vertexIndices[1]  = idxs[i] - 1;
        tri.textureIndices[1] = texCoords[i] - 1;
        tri.vertexIndices[2]  = idxs[i+1] - 1;
        tri.textureIndices[2] = texCoords[i+1] - 1;
        triangles.push_back(tri);
    }
    
    // Compute face normal and add contribution to each of the face's vertices.
    unsigned int i0 = tri.vertexIndices[0];
    unsigned int i1 = tri.vertexIndices[1];
    unsigned int i2 = tri.vertexIndices[2];
    
    vec3 n;
    compute_normal (clockwise, vertices[i0], vertices[i1], vertices[i2], n);
    
    for (int i = 0; i < idxs.size(); i++)
    {
        vec3_add (n, normals[idxs[i]-1]);
    }
}


Model_error_code OBJ_load (const char* filename, char clockwise, char left_handed, Model* model)
{
    vec3* norms     = 0;
    vec3* verts     = 0;
    texCoord* texcs = 0;
    triangle* tris  = 0;
    FILE* file      = 0;
    
    try
    {
        file = fopen (filename, "r");
        
        vec3 vert;
        vec3 normal;
        texCoord texc;
        
        std::vector<vec3>     vertices;
        std::vector<vec3>     normals;
        std::vector<texCoord> texCoords;
        std::vector<triangle> triangles;
        
        while (!feof(file))
        {
            switch (fgetc(file))
            {
            case 'v':
                switch (fgetc(file))
                {
                case 't':
                    read_tex_coord (file, texc);
                    texCoords.push_back(texc);
                    break;
                    
                case 'n':
                    read_normal (file, normal);
                    break;
                    
                default:
                    read_vertex (file, vert);
                    vertices.push_back(vert);
                    break;
                }
                break;
                
            case 'f':
                // If the normals vector has no size, initialise it.
                if (normals.size() == 0)
                {
                    vec3 zero;
                    zero.x = 0.0f; zero.y = 0.0f; zero.z = 0.0f;
                    normals = std::vector<vec3>(vertices.size(), zero);
                }
                read_face (file, clockwise, vertices, normals, triangles);
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
        
        unsigned int numVertices  = vertices.size();
        unsigned int numTexCoords = texCoords.size();
        unsigned int numTriangles = triangles.size();
        
        verts = new vec3 [numVertices];
        norms = new vec3 [numVertices];
        texcs = new texCoord [numTexCoords];
        tris  = new triangle [numTriangles];
        
        memcpy (verts, &vertices[0],  numVertices  * sizeof(vec3));
        memcpy (norms, &normals[0],   numVertices  * sizeof(vec3));
        memcpy (texcs, &texCoords[0], numTexCoords * sizeof(texCoord));
        memcpy (tris,  &triangles[0], numTriangles * sizeof(triangle));
        
        // Copy normals if the model file specified them.
        
        
        
        // Otherwise normalise the normals that have been previously computed.
        
        for (size_t i = 0; i < numVertices; ++i)
        {
            normalise(norms[i]);
        }
        
        model->vertices      = verts;
        model->normals       = norms;
        model->texCoords     = texcs;
        model->triangles     = tris;
        model->skins         = 0;
        model->animations    = 0;
        model->numFrames     = 1;
        model->numVertices   = numVertices;
        model->numTriangles  = numTriangles;
        model->numTexCoords  = numTexCoords;
        model->numSkins      = 0;
        model->numAnimations = 0;
        
        return Model_Success;
    }
    catch (std::bad_alloc)
    {
        safe_free (verts);
        safe_free (texcs);
        safe_free (tris);
        return Model_Memory_Allocation_Error;
    }
}
