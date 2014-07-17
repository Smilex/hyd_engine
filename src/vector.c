#include "vector.h"
#include <math.h>

float hyd_v2_dot_product(struct hyd_v2 lhs, struct hyd_v2 rhs)
{
	float r = (lhs.x * rhs.x) + (lhs.y * rhs.y);
	return r;
}

struct hyd_v2 hyd_v2_normalize(struct hyd_v2 vec)
{
	float length = hyd_v2_length(vec);
	struct hyd_v2 normVec = {
		.x = vec.x / length,
		.y = vec.y / length
	};
	return normVec;
}

float hyd_v2_length(struct hyd_v2 vec)
{
	return sqrtf((vec.x * vec.x) + (vec.y * vec.y));
}

struct hyd_v2 hyd_v2_substract(struct hyd_v2 lhs, struct hyd_v2 rhs)
{
	struct hyd_v2 d = {
		.x = lhs.x - rhs.x,
		.y = lhs.y - rhs.y
	};

	return d;
}

struct hyd_v2 hyd_v2_add(struct hyd_v2 lhs, struct hyd_v2 rhs)
{
	struct hyd_v2 r = {
		.x = lhs.x + rhs.x,
		.y = lhs.y + rhs.y
	};
	
	return r;
}
