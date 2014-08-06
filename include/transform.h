/**
 * \file transform.h
 */

#ifndef HYD_TRANSFORM_H
#define HYD_TRANSFORM_H

#include <stdint.h>
#include "vector.h"

struct hyd_transform {
	float mat[16];
};

void hyd_transform_ident(struct hyd_transform *t);

void hyd_transform_scale(struct hyd_transform *t, float sx, float sy, float sz);

void hyd_transform_ortho(struct hyd_transform *t, float w, float h, float near, float far);

struct hyd_transform
hyd_transform_mul(struct hyd_transform *lhs, struct hyd_transform *rhs);

struct hyd_v2
hyd_transform_mul_v2(struct hyd_transform *lhs, struct hyd_v2 *rhs);

void hyd_transform_translate(struct hyd_transform *t, float dx, float dy, float dz);

uint8_t hyd_transform_stack_push(struct hyd_transform t);

struct hyd_transform hyd_transform_stack_pop(void);

struct hyd_transform hyd_transform_get_applied(void);

#endif
