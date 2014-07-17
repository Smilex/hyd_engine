#include "matrix.h"

#include <stdio.h>

void MatrixIdentity4x4(float* matrix)
{
	*(matrix + 0) = 1.0f; *(matrix + 4) = 0.0f; *(matrix + 8) = 0.0f; *(matrix + 12) = 0.0f;
	*(matrix + 1) = 0.0f; *(matrix + 5) = 1.0f; *(matrix + 9) = 0.0f; *(matrix + 13) = 0.0f;
	*(matrix + 2) = 0.0f; *(matrix + 6) = 0.0f; *(matrix + 10) = 1.0f; *(matrix + 14) = 0.0f;
	*(matrix + 3) = 0.0f; *(matrix + 7) = 0.0f; *(matrix + 11) = 0.0f; *(matrix + 15) = 1.0f;
}

void MatrixScale4x4(float* matrix, float sx, float sy, float sz)
{
	*(matrix + 0) *= sx;
	*(matrix + 5) *= sy;
	*(matrix + 10) *= sz;
}

void MatrixTranslate4x4(float* matrix, float dx, float dy, float dz)
{
	*(matrix + 12) += dx;
	*(matrix + 13) += dy;
	*(matrix + 14) += dz;
}

void MatrixRotate4x4(float* matrix, float angle)
{
	*(matrix + 0) = cos(angle);
	*(matrix + 1) = sin(angle);
	*(matrix + 4) = -sin(angle);
	*(matrix + 5) = cos(angle);
}

void MatrixMultiply4x4(float* matrix1, float* matrix2, float* out)
{
	unsigned int outIter = 0;
	unsigned int mat1Iter = 0;
	unsigned int mat2Iter = 0;

	unsigned int i = 0;
	for (; i < 4; i++)
	{
		unsigned int j = 0;
		mat1Iter = 0;
		for (; j < 4; j++)
		{
			*(out + outIter) = (*(matrix1 + mat1Iter + 0)) * (*(matrix2 + mat2Iter + 0));
			*(out + outIter) += (*(matrix1 + mat1Iter + 4)) * (*(matrix2 + mat2Iter + 1));
			*(out + outIter) += (*(matrix1 + mat1Iter + 8)) * (*(matrix2 + mat2Iter + 2));
			*(out + outIter) += (*(matrix1 + mat1Iter + 12)) * (*(matrix2 + mat2Iter + 3));

			mat1Iter++;
			outIter++;
		}
		mat2Iter += 4;
	}
}

void MatrixPrint4x4(float* matrix)
{
	unsigned int i = 0;
	for(; i < 4; i++)
	{
		printf("[%.1f][%.1f][%.1f][%.1f]\n", *(matrix + i + 0),*(matrix + i + 4),*(matrix + i + 8),*(matrix + i + 12));
	}
}

void mat_ortho(float* matrix, float w, float h, float near, float far)
{
	*(matrix + 0) = 2 / w;
	*(matrix + 5) = 2 / -h;
	*(matrix + 10) = -2 / (far - near);
	*(matrix + 12) = - 1;
	*(matrix + 13) = 1;
	*(matrix + 14) = (far + near) / (far - near);
}
