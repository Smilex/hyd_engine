/**
 * \file quad.h
 */

#ifndef HYD_QUAD_H
#define HYD_QUAD_H

#include "color.h"
#include "vector.h"
#include "transform.h"
#include "texture.h"

struct hyd_quad {
	float x1, y1, x2, y2;
};

void hyd_quad_draw(struct hyd_quad *q, struct hyd_color *c);

void hyd_quad_tex_draw(struct hyd_quad *q, struct hyd_color *c,
		struct hyd_tex *t, struct hyd_quad *uv);

#endif
