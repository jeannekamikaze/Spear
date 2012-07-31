#include "OBJ_load.h"
#include <cstdio>


int main (void)
{
    Model model;
    OBJ_load ("/home/jeanne/assets/box.obj", 1, 1, &model);
    
    printf("Vertices:\n");
    
    for (size_t i = 0; i < model.numVertices; ++i)
    {
        vec3 v = model.vertices[i];
        printf ("%f, %f, %f\n", v.x, v.y, v.z);
    }
    
    printf("\nNormals:\n");
    
    for (size_t i = 0; i < model.numVertices; ++i)
    {
        vec3 n = model.normals[i];
        printf ("%f, %f, %f\n", n.x, n.y, n.z);
    }
    
    printf("\nTex coords:\n");
    
    for (size_t i = 0; i < model.numTexCoords; ++i)
    {
        texCoord tex = model.texCoords[i];
        printf("%f, %f\n", tex.s, tex.t);
    }
    
    printf("\nTriangles:\n");
    
    for (size_t i = 0; i < model.numTriangles; ++i)
    {
        triangle t = model.triangles[i];
        printf ("%d, %d, %d - %d, %d, %d\n",
            t.vertexIndices[0]+1,  t.vertexIndices[1]+1,  t.vertexIndices[2]+1,
            t.textureIndices[0]+1, t.textureIndices[1]+1, t.textureIndices[2]+1);
    }
    
    model_free (&model);
    
    return 0;
}
