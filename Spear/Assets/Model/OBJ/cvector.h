#ifndef _C_SPEAR_VECTOR_H
#define _C_SPEAR_VECTOR_H

typedef struct
{
    char* data;
    char* next;
    int   chunk_size;
    int   elem_size;
}
vector;

/// Construct a new vector.
/// Returns non-zero on error.
int vector_new (vector* v, int elem_size, int num_elems);

/// Free the vector.
void vector_free (vector* v);

/// Initialise every position to the given value.
void vector_initialise (vector* v, void* value);

/// Append an element.
/// Returns non-zero on error.
int vector_append (vector* v, void* elem);

/// Access the ith element.
void* vector_ith (vector* v, int i);

/// Return the number of elements in the vector.
int vector_size (vector* v);

/// Return the vector's capacity.
int vector_capacity (vector* v);

#endif // _C_SPEAR_VECTOR_H
