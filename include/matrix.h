#ifndef OGLUI_MATRIX_H
#define OGLUI_MATRIX_H

#include <math.h>

#define PI acos(-1)

void MatrixIdentity4x4(float* matrix);

void MatrixScale4x4(float* matrix, float sx, float sy, float sz);
void MatrixTranslate4x4(float* matrix, float dx, float dy, float dz);
void MatrixRotate4x4(float* matrix, float angle);
void MatrixMultiply4x4(float* matrix1, float* matrix2, float* out);
void MatrixPrint4x4(float* matrix);
void mat_ortho(float* matrix, float w, float h, float near, float far);

#endif
