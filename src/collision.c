#include "collision.h"

#include <math.h>
#include <float.h>

struct hyd_coll_obj *hyd_coll_obj_create_json(json_t *root,
		struct hyd_v2 *parent)
{
	if (!json_is_object(root))
		return NULL;

	size_t i;
	json_t *json_iter;
	json_t *json_points;
	struct hyd_coll_obj *col_obj;

	json_points = json_object_get(root, "points");
	if (!json_is_array(json_points))
		return NULL;

	col_obj = malloc(sizeof(*col_obj));
	if (col_obj == NULL)
		return NULL;
	col_obj->type = POLYGON;
	col_obj->num_points = json_array_size(json_points);
	col_obj->points = calloc(col_obj->num_points, sizeof(*col_obj->points));
	col_obj->parent = parent;
	col_obj->area = 0.0f;
	col_obj->radius = 0.0f;
	hyd_list_init(&col_obj->list);

	json_array_foreach(json_points, i, json_iter) {
		if (!json_is_array(json_iter) &&
				json_array_size(json_iter) < 2)
			continue;

		col_obj->points[i].x = json_number_value(
				json_array_get(json_iter, 0)
				);
		col_obj->points[i].y = json_number_value(
				json_array_get(json_iter, 1)
				);
	}

	col_obj->area = hyd_coll_obj_calc_area(col_obj);
	col_obj->center = hyd_coll_obj_calc_center(col_obj);
	col_obj->radius = hyd_coll_obj_calc_radius(col_obj);

	return col_obj;
}

uint8_t hyd_coll_obj_list_create_json(struct hyd_list *list, json_t *root,
		struct hyd_v2 *parent)
{
	if (!json_is_array(root))
		return 1;

	size_t index;
	json_t *iter_json;
	struct hyd_coll_obj *obj;

	json_array_foreach(root, index, iter_json)
	{
		obj = hyd_coll_obj_create_json(iter_json, parent);
		if (obj != NULL)
			hyd_list_append(&obj->list, list);
	}

	return 0;
}

void hyd_coll_obj_destroy(struct hyd_coll_obj *col_obj)
{
	if (col_obj == NULL)
		return;

	free(col_obj->points);
	free(col_obj);
}

void hyd_coll_obj_draw(struct hyd_coll_obj *col_obj,
		SDL_Renderer *renderer)
{
	if (col_obj == NULL || col_obj->num_points < 2)
		return;

	SDL_SetRenderDrawColor(renderer,
			0x00, 0xFF, 0x00, 0xFF);
	uint32_t i;
	SDL_Point p1, p2;
	for (i = 1; i < col_obj->num_points; i++)
	{
		p1.x = (int)round(col_obj->points[i - 1].x + col_obj->parent->x);
		p1.y = (int)round(col_obj->points[i - 1].y + col_obj->parent->y);
		p2.x = (int)round(col_obj->points[i].x + col_obj->parent->x);
		p2.y = (int)round(col_obj->points[i].y + col_obj->parent->y);
		SDL_RenderDrawLine(renderer,
				p1.x, p1.y,
				p2.x, p2.y
				);
	}

	p1 = p2;
	p2.x = (int)round(col_obj->points[0].x + col_obj->parent->x);
	p2.y = (int)round(col_obj->points[0].y + col_obj->parent->y);
	SDL_RenderDrawLine(renderer,
			p1.x, p1.y,
			p2.x, p2.y
			);

	SDL_SetRenderDrawColor(renderer,
			0xFF, 0x00, 0x00, 0xFF);
	SDL_RenderDrawPoint(renderer,
			(int)round(col_obj->center.x + col_obj->parent->x),
			(int)round(col_obj->center.y + col_obj->parent->y)
			);

	SDL_SetRenderDrawColor(renderer,
			0x00, 0x00, 0x00, 0xFF);
}

float hyd_coll_obj_calc_area(struct hyd_coll_obj *col_obj)
{
	float area;
	uint32_t i;
	for (i = 0; i < col_obj->num_points; i++) {
		float x = col_obj->points[i].x;
		float y = col_obj->points[i].y;
		float x1, y1;
		if (i + 1 == col_obj->num_points) {
			x1 = col_obj->points[0].x;
			y1 = col_obj->points[0].y;
		} else {
			x1 = col_obj->points[i + 1].x;
			y1 = col_obj->points[i + 1].y;
		}
		area += x * y1 - x1 * y;
	}
	area /= 2;

	return area;
}

float hyd_coll_obj_calc_radius(struct hyd_coll_obj *col_obj)
{
	if (col_obj == NULL ||
			col_obj->num_points == 0)
		return 0;

	struct hyd_v2 d = hyd_v2_substract(col_obj->points[0], col_obj->center);
	float radius = hyd_v2_length(d);
	uint32_t i;

	for (i = 1; i < col_obj->num_points; i++)
	{
		d = hyd_v2_substract(col_obj->points[i], col_obj->center);
		if (hyd_v2_length(d) > radius)
			radius = hyd_v2_length(d);
	}

	return radius;
}

struct hyd_v2 hyd_coll_obj_calc_center(struct hyd_coll_obj *col_obj)
{
	uint32_t i;
	struct hyd_v2 center = {
		.x = 0.0f,
		.y = 0.0f
	};
	for (i = 0; i < col_obj->num_points; i++) {
		float x = col_obj->points[i].x;
		float y = col_obj->points[i].y;
		float x1, y1;
		float f;
		if (i + 1 == col_obj->num_points) {
			x1 = col_obj->points[0].x;
			y1 = col_obj->points[0].y;
		} else {
			x1 = col_obj->points[i + 1].x;
			y1 = col_obj->points[i + 1].y;
		}

		f = (x * y1 - x1 * y);
		center.x += (x + x1) * f;
		center.y += (y + y1) * f;
	}
	center.x /= 6 * col_obj->area;
	center.y /= 6 * col_obj->area;

	return center;
}

void hyd_coll_obj_project(struct hyd_coll_obj *col_obj, struct hyd_v2 axis,
		float *min, float *max)
{
	if (col_obj == NULL ||
			col_obj->num_points == 0)
		return;

	struct hyd_v2 p = hyd_v2_add(col_obj->points[0], *col_obj->parent);
	float dot_product = hyd_v2_dot_product(p, axis);
	uint32_t i;
	*min = dot_product;
	*max = dot_product;

	for (i = 1; i < col_obj->num_points; i++)
	{
		p = hyd_v2_add(col_obj->points[i], *col_obj->parent);
		dot_product = hyd_v2_dot_product(p, axis);
		if (dot_product < *min)
			*min = dot_product;
		else if (dot_product > *max)
			*max = dot_product;
	}
}

struct hyd_coll
*hyd_coll_check(struct hyd_coll_obj *obj1, struct hyd_coll_obj *obj2,
		float rel_x, float rel_y)
{
	if (obj1 == NULL || obj2 == NULL)
		return NULL;

	uint32_t i,j;
	uint32_t num_pts1 = obj1->num_points;
	uint32_t num_pts2 = obj2->num_points;
	struct hyd_v2 edge;
	struct hyd_v2 p, p_next;
	float min1 = 0.0f, min2 = 0.0f, max1 = 0.0f, max2 = 0.0f;
	float interval = 0.0f;
	float min_interval = FLT_MAX;
	struct hyd_v2 translation_axis;
	float rel_proj;
	struct hyd_coll *coll = malloc(sizeof(*coll));
	coll->intersects = 1;
	coll->will_intersect = 1;
	coll->minimum_translation_vector.x = 0.0f;
	coll->minimum_translation_vector.y = 0.0f;
	struct hyd_v2 rel_vec = {
		.x = rel_x,
		.y = rel_y
	};

	struct hyd_v2 d_vec = hyd_v2_substract(
			hyd_v2_add(obj2->center, *obj2->parent),
		   	hyd_v2_add(obj1->center, *obj1->parent)
			);
	float d_vec_len = hyd_v2_length(d_vec);

	if (d_vec_len > (obj1->radius + obj2->radius)) {
		coll->intersects = 0;
		coll->will_intersect = 0;
		return coll;
	}

	for (i = 0; i < (num_pts1 + num_pts2); i++)
	{
		if (i < num_pts1) {
			p = obj1->points[i];
			if (i + 1 == num_pts1)
				p_next = obj1->points[0];
			else
				p_next = obj1->points[i + 1];
		} else {
			j = i - num_pts1;
			p = obj2->points[j];
			if (j + 1 == num_pts2)
				p_next = obj2->points[0];
			else
				p_next = obj2->points[j + 1];
		}
		edge = hyd_v2_substract(p_next, p);
		struct hyd_v2 axis = {
			.x = -edge.y,
			.y = edge.x
		};
		axis = hyd_v2_normalize(axis);

		hyd_coll_obj_project(obj1, axis, &min1, &max1);
		hyd_coll_obj_project(obj2, axis, &min2, &max2);
		if (min1 < min2)
			interval = min2 - max1;
		else
			interval = min1 - max2;

		if (interval > 0)
			coll->intersects = 0;

		rel_proj = hyd_v2_dot_product(rel_vec, axis);

		if (rel_proj < 0)
			min1 += rel_proj;
		else
			max1 += rel_proj;

		if (min1 < min2)
			interval = min2 - max1;
		else
			interval = min1 - max2;

		if (interval > 0)
			coll->will_intersect = 0;

		if (!coll->intersects && !coll->will_intersect)
			break;

		interval = abs(interval);
		if (interval < min_interval) {
			min_interval = interval;
			translation_axis = axis;

			struct hyd_v2 d = hyd_v2_substract(obj1->center, obj2->center);
			if (hyd_v2_dot_product(d, translation_axis) < 0) {
				translation_axis.x = -translation_axis.x;
			}
		}
	}

	if (coll->will_intersect) {
		coll->minimum_translation_vector.x
			= translation_axis.x * min_interval;
		coll->minimum_translation_vector.y
			= translation_axis.y * min_interval;
	}

	return coll;
}

struct hyd_coll
*hyd_coll_list_check(struct hyd_list *list, struct hyd_coll_obj *obj,
		float rel_x, float rel_y)
{
	struct hyd_coll *coll = NULL;
	struct hyd_coll_obj *iter;

	hyd_list_for_each_entry(iter, list, list)
	{
		coll = hyd_coll_check(iter, obj, rel_x, rel_y);
	}

	return coll;
}

struct hyd_coll
*hyd_coll_list_check_list(struct hyd_list *list1, struct hyd_list *list2,
		float rel_x, float rel_y)
{
	struct hyd_coll *coll = NULL;
	struct hyd_coll_obj *iter;

	hyd_list_for_each_entry(iter, list2, list)
	{
		coll = hyd_coll_list_check(list1, iter,
				rel_x, rel_y);
	}

	return coll;
}

void hyd_coll_destroy(struct hyd_coll *coll)
{
	free(coll);
}

uint8_t hyd_coll_get_intersects(struct hyd_coll *coll)
{
	return coll->intersects;
}

uint8_t hyd_coll_get_will_intersect(struct hyd_coll *coll)
{
	return coll->will_intersect;
}

float hyd_coll_get_mtv_x(struct hyd_coll *coll)
{
	return coll->minimum_translation_vector.x;
}

float hyd_coll_get_mtv_y(struct hyd_coll *coll)
{
	return coll->minimum_translation_vector.y;
}
