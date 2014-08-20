module hyd.transform;

extern (C) nothrow {

struct hyd_transform {
	float mat[16];
}

void hyd_transform_ident(hyd_transform *t);

void hyd_transform_scale(hyd_transform *t, float sx, float sy, float sz);

void hyd_transform_ortho(hyd_transform *t, float w, float h, float near, float far);

hyd_transform hyd_transform_mul(hyd_transform *lhs, hyd_transform *rhs);

void hyd_transform_translate(hyd_transform *t, float dx, float dy, float dz);

bool hyd_transform_stack_push(hyd_transform t);

hyd_transform hyd_transform_stack_pop();

}
