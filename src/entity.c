#include "entity.h"

#include <string.h>
#include <math.h>
#include <stdlib.h>
#include "filesystem.h"
#include "stretchy_buffer.h"
#include "layer.h"

struct hyd_ent *hyd_ent_create(struct hyd_spr *spr, const char *n,
		struct hyd_ent *parent, struct hyd_layer *layer
		)
{
	struct hyd_ent *ent = malloc(sizeof(*ent));
	ent->spr = spr;
	if (n != NULL) {
		size_t len = strlen(n);
		ent->name = malloc(len + 1);
		strcpy(ent->name, n);
	} else
		ent->name = NULL;

	ent->coll.x1 = 0;
	ent->coll.y1 = 0;
	ent->coll.x2 = 0;
	ent->coll.y2 = 0;
	ent->children = malloc(sizeof(*ent->children));
	ent->children->next = ent->children;
	ent->properties = malloc(sizeof(*ent->properties));
	ent->properties->next = ent->properties;
	ent->next = NULL;
	ent->pos.x = 0.0f;
	ent->pos.y = 0.0f;
	ent->layer = layer;
	ent->parent = parent;
	if (parent != NULL) {
		ent->next = ent->parent->children->next;
		ent->parent->children->next = ent;
	}

	return ent;
}

void hyd_ent_draw(struct hyd_ent *ent)
{
	if (ent == NULL)
		return;

	SDL_Point pos;
	pos.x = round(ent->pos.x);
	pos.y = round(ent->pos.y);
	hyd_spr_draw_point(ent->spr, pos);

	struct hyd_ent *i;

	for (i = ent->children->next; i != ent->children; i = i->next)
	{
		hyd_ent_draw(i);
	}
}

void hyd_ent_destroy(struct hyd_ent *e)
{
	if (e == NULL)
		return;

	struct hyd_ent *ch_i, *ch_n;
	for (ch_i = e->children->next, ch_n = ch_i->next;
			ch_i != e->children;
			ch_i = ch_n, ch_n = ch_i->next)
	{
		hyd_ent_destroy(ch_i);
	}

	struct hyd_property *p_i, *p_n;
	for (p_i = e->properties->next, p_n = p_i->next;
			p_i != e->properties;
			p_i = p_n, p_n = p_i->next)
	{
		hyd_property_destroy(p_i);
	}

	hyd_spr_destroy(e->spr);
	free(e->children);
	free(e->properties);
	free(e->name);
	free(e);
}

struct hyd_ent *hyd_ent_copy(struct hyd_ent *e) {
	struct hyd_property *p_i, *p;
	struct hyd_ent *ret = malloc(sizeof(*ret));
	if (ret == NULL)
		return NULL;

	memcpy(ret, e, sizeof(*e));
	ret->spr = hyd_spr_copy(e->spr);
	ret->next = e->next;
	e->next = ret;
	ret->children = malloc(sizeof(*ret->children));
	ret->children->next = ret->children;
	ret->name = malloc(strlen(e->name) + 1);
	strcpy(ret->name, e->name);

	ret->properties = malloc(sizeof(*ret->properties));
	ret->properties->next = ret->properties;
	for (p_i = e->properties->next; p_i != e->properties; p_i = p_i->next) {
		p = malloc(sizeof(*p));
		memcpy(p, p_i, sizeof(*p_i));
		p->name = malloc(strlen(p_i->name) + 1);
		strcpy(p->name, p_i->name);
		p->next = ret->properties->next;
		ret->properties->next = p;
	}

	return ret;
}

enum HYD_ENT_COLL hyd_ent_coll(struct hyd_ent *l, struct hyd_ent *r) {
	struct hyd_v2 l_c = {
		l->pos.x + l->coll.x1 + (l->coll.x2 - l->coll.x1) / 2,
		l->pos.y + l->coll.y1 + (l->coll.y2 - l->coll.y1) / 2
	};
	struct hyd_v2 r_c = {
		r->pos.x + r->coll.x1 + (r->coll.x2 - r->coll.x1) / 2,
		r->pos.y + r->coll.y1 + (r->coll.y2 - r->coll.y1) / 2
	};

	float w = (l->coll.x2 - l->coll.x1) / 2 + (r->coll.x2 - r->coll.x1) / 2;
	float h = (l->coll.y2 - l->coll.y1) / 2 + (r->coll.y2 - r->coll.y1) / 2;

	enum HYD_ENT_COLL ret = C_NONE;
	if (r_c.x > l_c.x - w && r_c.x < l_c.x + w && r_c.y > l_c.y - h && r_c.y < l_c.y + h) {
		if (r_c.x < l_c.x)
			ret |= C_LEFT;
		if (r_c.x > l_c.x)
			ret |= C_RIGHT;
		if (r_c.y < l_c.y)
			ret |= C_TOP;
		if (r_c.y > l_c.y)
			ret |= C_BOTTOM;
	}

	return ret;
}

uint8_t hyd_ent_create_json_arr(struct hyd_ent *ent_list, json_t *root,
		struct hyd_tex_list *tex_l, struct hyd_ent *parent, struct hyd_layer *layer)
{
	if (!json_is_array(root)) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"Can't create entities. JSON is not array."
				);
		return 1;
	}

	uint32_t i = 0;
	struct hyd_ent *ent;
	json_t *arr_json = NULL;
	json_t *obj_json = NULL;

	for (i = 0; i < json_array_size(root); i++) {
		struct hyd_v2 position = {0, };

		arr_json = json_array_get(root, i);
		if (!json_is_object(arr_json)) {
			SDL_LogWarn(
					SDL_LOG_CATEGORY_APPLICATION,
					"Element in entity array is not JSON object."
					);
			continue;
		}

		obj_json = json_object_get(arr_json, "entity");
		if (json_is_string(obj_json)) {
			ent = hyd_ent_create_file(json_string_value(obj_json),
						tex_l, parent, layer);

			ent->next = ent_list->next;
			ent_list->next = ent;
		} else if (json_is_object(obj_json)) {
			ent = hyd_ent_create_json(obj_json,
					tex_l, parent, layer);

			ent->next = ent_list->next;
			ent_list->next = ent;
		}
		else
			continue;

		obj_json = json_object_get(arr_json, "x");
		if (json_is_number(obj_json))
			ent->pos.x = json_number_value(obj_json);

		obj_json = json_object_get(arr_json, "y");
		if (json_is_number(obj_json))
			ent->pos.y = json_number_value(obj_json);

		obj_json = json_object_get(arr_json, "sprite");
		if (json_is_string(obj_json)) {
			if (ent->spr != NULL)
				hyd_spr_destroy(ent->spr);
			ent->spr = hyd_spr_create_file(json_string_value(obj_json),
					tex_l);
		}

		obj_json = json_object_get(arr_json, "properties");
		if (json_is_object(obj_json))
			hyd_property_create_json(ent->properties, obj_json);
	}

	return 0;
}

struct hyd_ent *hyd_ent_create_json(json_t *root, struct hyd_tex_list *tex_l,
		struct hyd_ent *parent, struct hyd_layer *layer)
{
	if (!json_is_object(root)) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"Can't create entity. JSON is not object."
				);
		return NULL;
	}

	json_t *iter_json = NULL;

	const char *name = NULL;
	struct hyd_ent *ent = NULL;
	struct hyd_spr *spr = NULL;

	iter_json = json_object_get(root, "name");
	if (json_is_string(iter_json))
		name = json_string_value(iter_json);

	iter_json = json_object_get(root, "sprite");
	if (json_is_string(iter_json))
		spr = hyd_spr_create_file(json_string_value(iter_json),
				tex_l);

	ent = hyd_ent_create(spr, name, parent, layer);

	iter_json = json_object_get(root, "children");
	if (json_is_array(iter_json))
		hyd_ent_create_json_arr(ent->children,
				iter_json, tex_l, ent, layer);

	iter_json = json_object_get(root, "properties");
	if (json_is_object(iter_json))
		hyd_property_create_json(ent->properties, iter_json);

	iter_json = json_object_get(root, "collision");
	if (json_is_object(iter_json)) {
		json_t *c_i = json_object_get(iter_json, "x1");
		if (json_is_number(c_i))
			ent->coll.x1 = json_number_value(c_i);

		c_i = json_object_get(iter_json, "y1");
		if (json_is_number(c_i))
			ent->coll.y1 = json_number_value(c_i);

		c_i = json_object_get(iter_json, "x2");
		if (json_is_number(c_i))
			ent->coll.x2 = json_number_value(c_i);

		c_i = json_object_get(iter_json, "y2");
		if (json_is_number(c_i))
			ent->coll.y2 = json_number_value(c_i);
	}

	return ent;
}

struct hyd_ent *hyd_ent_create_file(const char *fname,
		struct hyd_tex_list *tex_l, struct hyd_ent *parent, struct hyd_layer *layer)
{
	uint8_t *buf = NULL;
	PHYSFS_sint64 read_len = 0;
	json_t *root = NULL;
	json_error_t err;
	struct hyd_ent *ent = NULL;

	read_len = hyd_fs_read_buffer(fname, &buf);
	if (read_len == 0) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"Got no data from '%s'",
				fname
				);
		return NULL;
	}

	//SDL_Log("Reading object - %s - size: %i", fname, read_len);

	root = json_loadb(buf, read_len, 0, &err);
	free(buf);

	if (root == NULL) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"JSON error for file '%s' - line: %d - message: %s.",
				fname, err.line, err.text
				);
		return NULL;
	}

	ent = hyd_ent_create_json(root, tex_l, parent, layer);
	json_decref(root);
	return ent;
}

float hyd_ent_get_number_property(struct hyd_ent *ent, const char *n)
{
	struct hyd_property *i;
	for (i = ent->properties->next; i != ent->properties; i = i->next)
	{
		if (strcmp(i->name, n) == 0)
			return i->value.n;
	}

	return 0.0f;
}

uint8_t hyd_ent_get_bool_property(struct hyd_ent *ent, const char *n)
{
	struct hyd_property *i;
	for (i = ent->properties->next; i != ent->properties; i = i->next)
	{
		if (strcmp(i->name, n) == 0)
			return i->value.b;
	}

	return 0;
}

const char *hyd_ent_get_string_property(struct hyd_ent *ent, const char *n)
{
	struct hyd_property *i;
	for (i = ent->properties->next; i != ent->properties; i = i->next)
	{
		if (strcmp(i->name, n) == 0)
			return i->value.s;
	}

	return "";
}

void hyd_ent_set_number_property(struct hyd_ent *ent, float value, const char *n)
{
	struct hyd_property *i;
	for (i = ent->properties->next; i != ent->properties; i = i->next)
	{
		if (strcmp(i->name, n) == 0) {
			i->type = NUMBER;
			i->value.n = value;
			return;
		}
	}

	struct hyd_property *p = hyd_property_create_number(value, n);
	p->next = ent->properties->next;
	ent->properties->next = p;
}

void hyd_ent_set_bool_property(struct hyd_ent *ent, uint8_t value, const char *n)
{
	struct hyd_property *i;
	for (i = ent->properties->next; i != ent->properties; i = i->next)
	{
		if (strcmp(i->name, n) == 0) {
			i->type = BOOL;
			i->value.b = value;
			return;
		}
	}

	struct hyd_property *p = hyd_property_create_bool(value, n);
	p->next = ent->properties->next;
	ent->properties->next = p;
}

void hyd_ent_set_string_property(struct hyd_ent *ent, const char *value, const char *n)
{
	struct hyd_property *i;
	for (i = ent->properties->next; i != ent->properties; i = i->next)
	{
		if (strcmp(i->name, n) == 0) {
			i->type = STRING;
			free(i->value.s);
			i->value.s = malloc(strlen(value) + 1);
			strcpy(i->value.s, value);
			return;
		}
	}

	struct hyd_property *p = hyd_property_create_string(value, n);
	p->next = ent->properties->next;
	ent->properties->next = p;
}

struct hyd_ent **hyd_ent_list_find(struct hyd_ent *l, const char *n, uint32_t *num)
{
	struct hyd_ent **ret = NULL;
	struct hyd_ent *i;
	*num = 0;

	for (i = l->next; i != l; i = i->next) {
		if (i->name == NULL)
			continue;
		if (strcmp(i->name, n) == 0) {
			sb_push(ret, i);
			(*num)++;
		}
	}

	return ret;
}

struct hyd_ent *hyd_ent_list_find_pos(struct hyd_ent *l, float x, float y) {
	struct hyd_ent *i;
	for (i = l->next; i != l; i = i->next) {
		SDL_Rect r = i->spr->frames[0]->rect;
		r.x = i->pos.x;
		r.y = i->pos.y;
		if ((x > r.x && x < (r.x + r.w)) && (y > r.y && y < (r.y + r.h)))
			return i;
	}

	return NULL;
}
