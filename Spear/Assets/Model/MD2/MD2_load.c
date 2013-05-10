#include "MD2_load.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h> // malloc
#include <math.h> // sqrt

//! The MD2 magic number used to identify MD2 files.
#define MD2_ID  0x32504449

//! Limit values for the MD2 file format.
#define MD2_MAX_TRIANGLES   4096
#define MD2_MAX_VERTICES    2048
#define MD2_MAX_TEXCOORDS   2048
#define MD2_MAX_FRAMES       512
#define MD2_MAX_SKINS         32


/// MD2 file header.
typedef struct
{
    I32     magic;                  /// The magic number "IDP2"; 844121161 in decimal; 0x32504449
    I32     version;                /// Version number, always 8.
    I32     skinWidth;              /// Width of the skin(s) in pixels.
    I32     skinHeight;             /// Height of the skin(s) in pixels.
    I32     frameSize;              /// Size of a single frame in bytes.
    I32     numSkins;               /// Number of skins.
    I32     numVertices;            /// Number of vertices in a single frame.
    I32     numTexCoords;           /// Number of texture coordinates.
    I32     numTriangles;           /// Number of triangles.
    I32     numGlCommands;          /// Number of dwords in the Gl command list.
    I32     numFrames;              /// Number of frames.
    I32     offsetSkins;            /// Offset from the start of the file to the array of skins.
    I32     offsetTexCoords;        /// Offset from the start of the file to the array of texture coordinates.
    I32     offsetTriangles;        /// Offset from the start of the file to the array of triangles.
    I32     offsetFrames;           /// Offset from the start of the file to the array of frames.
    I32     offsetGlCommands;       /// Offset from the start of the file to the array of Gl commands.
    I32     offsetEnd;              /// Offset from the start of the file to the end of the file (the file size).
}
md2Header_t;


/// Represents a texture coordinate index.
typedef struct
{
    I16 s;
    I16 t;
}
texCoord_t;


/// Represents a frame point.
typedef struct
{
    U8 x, y, z;
    U8 lightNormalIndex;
}
vertex_t;


/// Represents a single frame.
typedef struct
{
    vec3     scale;
    vec3     translate;
    I8        name[16];
    vertex_t vertices[1];
}
frame_t;


static void normalise (vec3* v)
{
    float x = v->x;
    float y = v->y;
    float z = v->z;
    float mag = sqrt (x*x + y*y + z*z);
    mag = mag == 0 ? 1 : mag;
    v->x = x / mag;
    v->y = y / mag;
    v->z = z / mag;
}


static void cross (const vec3* a, const vec3* b, vec3* c)
{
    c->x = a->y * b->z - a->z * b->y;
    c->y = a->z * b->x - a->x * b->z;
    c->z = a->x * b->y - a->y * b->x;
}


static void vec3_sub (const vec3* a, const vec3* b, vec3* out)
{
    out->x = a->x - b->x;
    out->y = a->y - b->y;
    out->z = a->z - b->z;
}


static void normal (char clockwise, const vec3* p1, const vec3* p2, const vec3* p3, vec3* n)
{
    vec3 v1, v2;
    if (clockwise)
    {
        vec3_sub (p3, p2, &v1);
        vec3_sub (p1, p2, &v2);
    }
    else
    {
        vec3_sub (p1, p2, &v1);
        vec3_sub (p3, p2, &v2);
    }
    cross (&v1, &v2, n);
    normalise (n);
}


typedef struct
{
    vec3* normals;
    vec3* base;
    unsigned int N;
}
normal_map;


static void normal_map_initialise (normal_map* m, unsigned int N)
{
    m->N = N;
}


static void normal_map_clear (normal_map* m, vec3* normals, vec3* base)
{
    memset (normals, 0, m->N * sizeof(vec3));
    m->normals = normals;
    m->base    = base;
}


static void normal_map_insert (normal_map* m, vec3* vec, vec3 normal)
{
    unsigned int i = vec - m->base;
    vec3* n = m->normals + i;
    n->x += normal.x;
    n->y += normal.y;
    n->z += normal.z;
}


static void compute_normals (normal_map* m, char left_handed)
{
    vec3* n = m->normals;
    unsigned int i;
    for (i = 0; i < m->N; ++i)
    {
        if (!left_handed)
        {
            n->x = -n->x;
            n->y = -n->y;
            n->z = -n->z;
        }
        normalise (n);
        n++;
    }
}


static void safe_free (void* ptr)
{
    if (ptr) free (ptr);
}


static char frame_equal (const char* name1, const char* name2)
{
    char equal = 1;
    int i;

    if (((name1 == 0) && (name2 != 0)) || ((name1 != 0) && (name2 == 0)))
    {
        return 0;
    }

    for (i = 0; i < 16; ++i)
    {
        char c1 = *name1;
        char c2 = *name2;
        if ((c1 >= '0' && c1 <= '9') || (c2 >= '0' && c2 <= '9')) break;
        if (c1 != c2)
        {
            equal = 0;
            break;
        }
        if (c1 == '_' || c2 == '_') break;
        name1++;
        name2++;
    }
    return equal;
}


static void animation_remove_numbers (char* name)
{
    int i;
    for (i = 0; i < 16; ++i)
    {
        char c = *name;
        if (c == 0) break;
        if (c >= '0' && c <= '9') *name = 0;
        name++;
    }
}


Model_error_code MD2_load (const char* filename, char clockwise, char left_handed, Model* model)
{
    FILE*       filePtr;
    vec3*       vertices;
    vec3*       normals;
    texCoord*   texCoords;
    triangle*   triangles;
    skin*       skins;
    animation* animations;
    int i;

    // Open the file for reading.
    filePtr = fopen(filename, "rb");
    if (!filePtr) return Model_File_Not_Found;

    // Make sure it is an MD2 file.
    int magic;
    if ((fread(&magic, 4, 1, filePtr)) != 1)
    {
        fclose(filePtr);
        return Model_Read_Error;
    }

    if (magic != MD2_ID) return Model_File_Mismatch;

    // Find out the file size.
    long int fileSize;
    fseek(filePtr, 0, SEEK_END);
    fileSize = ftell(filePtr);
    fseek(filePtr, 0, SEEK_SET);

    // Allocate a chunk of data to store the file in.
    char *buffer = (char*) malloc(fileSize);
    if (!buffer)
    {
        fclose(filePtr);
        return Model_Memory_Allocation_Error;
    }

    // Read the entire file into memory.
    if ((fread(buffer, 1, fileSize, filePtr)) != (unsigned int)fileSize)
    {
        fclose(filePtr);
        free(buffer);
        return Model_Read_Error;
    }

    // File stream is no longer needed.
    fclose(filePtr);

    // Set a pointer to the header for parsing.
    md2Header_t* header = (md2Header_t*) buffer;

    // Compute the number of animations.
    unsigned numAnimations = 1;
    int currentFrame;
    const char* name = 0;
    for (currentFrame = 0; currentFrame < header->numFrames; ++currentFrame)
    {
        frame_t* frame = (frame_t*) &buffer[header->offsetFrames + currentFrame * header->frameSize];
        if (name == 0)
        {
            name = frame->name;
        }
        else if (!frame_equal(name, frame->name))
        {
            numAnimations++;
            name = frame->name;
        }
    }

    // Allocate memory for arrays.
    vertices   = (vec3*) malloc(sizeof(vec3) * header->numVertices * header->numFrames);
    normals    = (vec3*) malloc(sizeof(vec3) * header->numVertices * header->numFrames);
    texCoords  = (texCoord*) malloc(sizeof(texCoord) * header->numTexCoords);
    triangles  = (triangle*) malloc(sizeof(triangle) * header->numTriangles);
    skins      = (skin*) malloc(sizeof(skin) * header->numSkins);
    animations = (animation*) malloc (numAnimations * sizeof(animation));

    if (!vertices || !normals || !texCoords || !triangles || !skins || !animations)
    {
        safe_free (animations);
        safe_free (skins);
        safe_free (triangles);
        safe_free (texCoords);
        safe_free (normals);
        safe_free (vertices);
        free (buffer);
        return Model_Memory_Allocation_Error;
    }

    // Load the model's vertices.
    // Loop through each frame, grab the vertices that make it up, transform them back
    // to their real coordinates and store them in the model's vertex array.
    for (currentFrame = 0; currentFrame < header->numFrames; ++currentFrame)
    {
        // Set a frame pointer to the current frame.
        frame_t* frame = (frame_t*) &buffer[header->offsetFrames + currentFrame * header->frameSize];

        // Set a vertex pointer to the model's vertex array, at the appropiate position.
        vec3* vert = &vertices[currentFrame * header->numVertices];

        // Now parse those vertices and transform them back.
        int currentVertex;
        for (currentVertex = 0; currentVertex != header->numVertices; ++currentVertex)
        {
            vert[currentVertex].x = frame->vertices[currentVertex].x * frame->scale.x + frame->translate.x;
            vert[currentVertex].y = frame->vertices[currentVertex].y * frame->scale.y + frame->translate.y;
            vert[currentVertex].z = frame->vertices[currentVertex].z * frame->scale.z + frame->translate.z;
        }
    }

    // Load the model's triangles.

    // Set a pointer to the triangles array in the buffer.
    triangle* t = (triangle*) &buffer[header->offsetTriangles];

    if (clockwise)
    {
        for (i = 0; i < header->numTriangles; ++i)
        {
            triangles[i].vertexIndices[0]  = t[i].vertexIndices[0];
            triangles[i].vertexIndices[1]  = t[i].vertexIndices[1];
            triangles[i].vertexIndices[2]  = t[i].vertexIndices[2];

            triangles[i].textureIndices[0] = t[i].textureIndices[0];
            triangles[i].textureIndices[1] = t[i].textureIndices[1];
            triangles[i].textureIndices[2] = t[i].textureIndices[2];
        }
    }
    else
    {
        for (i = 0; i < header->numTriangles; ++i)
        {
            triangles[i].vertexIndices[0]  = t[i].vertexIndices[0];
            triangles[i].vertexIndices[1]  = t[i].vertexIndices[2];
            triangles[i].vertexIndices[2]  = t[i].vertexIndices[1];

            triangles[i].textureIndices[0] = t[i].textureIndices[0];
            triangles[i].textureIndices[1] = t[i].textureIndices[2];
            triangles[i].textureIndices[2] = t[i].textureIndices[1];
        }
    }

    // Load the texture coordinates.
    float sw = (float) header->skinWidth;
    float sh = (float) header->skinHeight;
    texCoord_t* texc = (texCoord_t*) &buffer[header->offsetTexCoords];
    for (i = 0; i < header->numTexCoords; ++i)
    {
        texCoords[i].s  = (float)texc->s / sw;
        texCoords[i].t  = 1.0f - (float)texc->t / sh;
        texc++;
    }

    // Iterate over every frame and compute normals for every triangle.
    vec3 n;

    normal_map map;
    normal_map_initialise (&map, header->numVertices);

    for (currentFrame = 0; currentFrame < header->numFrames; ++currentFrame)
    {
        // Set a pointer to the triangle array.
        triangle* t = triangles;

        // Set a pointer to the vertex array at the appropiate position.
        vec3* vertex_array = vertices + header->numVertices * currentFrame;

        // Set a pointer to the normals array at the appropiate position.
        vec3* normals_ptr = normals + header->numVertices * currentFrame;

        normal_map_clear (&map, normals_ptr, vertex_array);

        for (i = 0; i < header->numTriangles; ++i)
        {
            // Compute face normal.
            vec3* v0 = &vertex_array[t->vertexIndices[0]];
            vec3* v1 = &vertex_array[t->vertexIndices[1]];
            vec3* v2 = &vertex_array[t->vertexIndices[2]];
            normal (clockwise, v0, v1, v2, &n);

            // Add face normal to each of the face's vertices.
            normal_map_insert (&map, v0, n);
            normal_map_insert (&map, v1, n);
            normal_map_insert (&map, v2, n);

            t++;
        }

        compute_normals (&map, left_handed);
    }

    // Load the model's skins.
    const skin* s = (const skin*) &buffer[header->offsetSkins];
    for (i = 0; i < header->numSkins; ++i)
    {
        memcpy (skins[i].name, s->name, 64);
        s++;
    }

    // Load the model's animations.
    unsigned start = 0;
    name = 0;
    animation* currentAnimation = animations;
    for (currentFrame = 0; currentFrame < header->numFrames; ++currentFrame)
    {
        frame_t* frame = (frame_t*) &buffer[header->offsetFrames + currentFrame * header->frameSize];
        if (name == 0)
        {
            name = frame->name;
        }
        else if (!frame_equal(name, frame->name))
        {
            memcpy (currentAnimation->name, name, 16);
            animation_remove_numbers (currentAnimation->name);
            currentAnimation->start = start;
            currentAnimation->end   = currentFrame-1;
            if (currentAnimation != animations)
            {
                animation* prev = currentAnimation;
                prev--;
                prev->end = start-1;
            }
            name = frame->name;
            currentAnimation++;
            start = currentFrame;
        }
    }
    currentAnimation = animations + numAnimations - 1;
    memcpy (currentAnimation->name, name, 16);
    animation_remove_numbers (currentAnimation->name);
    currentAnimation->start = start;
    currentAnimation->end   = header->numFrames-1;

    /*printf ("finished loading model %s\n", filename);
    printf ("numAnimations: %u\n", numAnimations);
    printf ("animations: %p\n", animations);

    currentAnimation = animations;
    for (i = 0; i < numAnimations; ++i)
    {
        printf ("Animation %d, name: %s, start: %d, end %d\n",
            i, currentAnimation->name, currentAnimation->start, currentAnimation->end);
        currentAnimation++;
    }*/
    
    model->vertices   = vertices;
    model->normals    = normals;
    model->texCoords  = texCoords;
    model->triangles  = triangles;
    model->skins      = skins;
    model->animations = animations;

    model->numFrames     = header->numFrames;
    model->numVertices   = header->numVertices;
    model->numTriangles  = header->numTriangles;
    model->numTexCoords  = header->numTexCoords;
    model->numSkins      = header->numSkins;
    model->numAnimations = numAnimations;

    free(buffer);

    return Model_Success;
}
