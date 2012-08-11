#include "cvector.h"
#include <stdlib.h> // malloc, realloc, free
#include <string.h> // memcpy


int max (int a, int b)
{
    if (a > b) return a;
    return b;
}


int vector_new (vector* v, int elem_size, int num_elems)
{
    int n = num_elems * elem_size;
    
    char* data = 0;
    if (num_elems > 0)
    {
        data = (char*) malloc (n);
        if (data == NULL) return 1;
    }
    
    v->data       = data;
    v->next       = data;
    v->chunk_size = n;
    v->elem_size  = elem_size;
    
    return 0;
}


void vector_free (vector* v)
{
    if (v->data != 0) free (v->data);
}


void vector_initialise (vector* v, void* value)
{
    char* ptr = v->data;
    int esize = v->elem_size;
    int n = vector_size (v);
    
    int i;
    for (i = 0; i < n; ++i)
    {
        memcpy (ptr, value, esize);
        ptr += esize;
    }
}


int vector_append (vector* v, void* elem)
{
    // Realloc a bigger chunk when the vector runs out of space.
    if (v->next == v->data + v->chunk_size)
    {
        int old_chunk_size = v->chunk_size;
        int n = max (v->elem_size, 2 * old_chunk_size);
        
        char* data = (char*) realloc (v->data, n);
        if (data == NULL) return 1;
        
        v->data = data;
        v->next = data + old_chunk_size;
        v->chunk_size = n;
    }
    
    memcpy ((void*)v->next, elem, v->elem_size);
    v->next += v->elem_size; 
}


void* vector_ith (vector* v, int i)
{
    return (void*) (v->data + i*v->elem_size);
}


int vector_size (vector* v)
{
    return (v->next - v->data) / v->elem_size;
}


int vector_capacity (vector* v)
{
    return v->chunk_size / v->elem_size;
}
