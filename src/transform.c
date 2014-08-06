#include "transform.h"
#include <string.h>

#define _HYD_TRANS_STACK_MAX 25
struct hyd_transform _hyd_trans_applied = {0};
struct hyd_transform _hyd_trans_stack[_HYD_TRANS_STACK_MAX];
uint8_t _hyd_trans_stack_size = 0;
uint8_t _hyd_trans_changed = 0x1;

void hyd_transform_ident(struct hyd_transform *t) {
	memset(t->mat, 0, 16 * sizeof(float));
	t->mat[0] = 1.0f;
	t->mat[5] = 1.0f;
	t->mat[10] = 1.0f;
	t->mat[15] = 1.0f;
}

void hyd_transform_scale(struct hyd_transform *t, float sx, float sy, float sz) {
	t->mat[0] *= sx;
	t->mat[5] *= sx;
	t->mat[10] *= sx;
}

void hyd_transform_ortho(struct hyd_transform *t, float w, float h, float near, float far) {
	t->mat[0] = 2 / w;
	t->mat[5] = 2 / -h;
	t->mat[10] = -2 / (far - near);
	t->mat[12] = - 1;
	t->mat[13] = 1;
	t->mat[14] = (far + near) / (far - near);
}

struct hyd_transform hyd_transform_mul(struct hyd_transform *lhs,
		struct hyd_transform *rhs) {
	uint32_t out_i = 0;
	uint32_t t1_i = 0;
	uint32_t t2_i = 0;
	struct hyd_transform out = {0};

	uint32_t i = 0;
	for (; i < 4; i++)
	{
		uint32_t j = 0;
		t1_i = 0;
		for (; j < 4; j++)
		{
			out.mat[out_i] = lhs->mat[t1_i + 0] * rhs->mat[t2_i + 0];
			out.mat[out_i] += lhs->mat[t1_i + 4] * rhs->mat[t2_i + 1];
			out.mat[out_i] += lhs->mat[t1_i + 8] * rhs->mat[t2_i + 2];
			out.mat[out_i] += lhs->mat[t1_i + 12] * rhs->mat[t2_i + 3];

			t1_i++;
			out_i++;
		}
		t2_i += 4;
	}

	return out;
}

struct hyd_v2
hyd_transform_mul_v2(struct hyd_transform *lhs, struct hyd_v2 *rhs) {
	struct hyd_v2 ret;
	ret.x = lhs->mat[0] * rhs->x + lhs->mat[4] * rhs->y + lhs->mat[12];
	ret.y = lhs->mat[1] * rhs->x + lhs->mat[5] * rhs->y + lhs->mat[13];

	return ret;
}

void hyd_transform_translate(struct hyd_transform *t, float dx, float dy, float dz) {
	t->mat[12] += dx;
	t->mat[13] += dy;
	t->mat[14] += dz;
}

uint8_t hyd_transform_stack_push(struct hyd_transform t) {
	if (_hyd_trans_stack_size == _HYD_TRANS_STACK_MAX)
		return 1;
	_hyd_trans_stack[_hyd_trans_stack_size] = t;
	_hyd_trans_stack_size++;
	_hyd_trans_changed = 0x1;
	return 0;
}

struct hyd_transform hyd_transform_stack_pop(void) {
	_hyd_trans_changed = 0x1;
	struct hyd_transform ret = _hyd_trans_stack[_hyd_trans_stack_size];
	_hyd_trans_stack_size--;
	return ret;
}

struct hyd_transform hyd_transform_get_applied(void) {
	if (_hyd_trans_changed) {
		uint8_t i;
		hyd_transform_ident(&_hyd_trans_applied);
		for (i = 0; i < _hyd_trans_stack_size; i++) {
			_hyd_trans_applied = hyd_transform_mul(&_hyd_trans_applied, &_hyd_trans_stack[i]);
		}
		_hyd_trans_changed = 0x0;
	}

	return _hyd_trans_applied;
}
