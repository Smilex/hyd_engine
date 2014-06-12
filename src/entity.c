#include "entity.h"

#include "filesystem.h"
#include <string.h>
#include <math.h>
#include <stdlib.h>

struct hyd_ent *hyd_ent_create(struct hyd_spr *sprite, const char *name,
		struct hyd_ent *parent
		)
{
	struct hyd_ent *ent = malloc(sizeof(*ent));
	ent->spr = sprite;
	if (name != NULL) {
		size_t len = strlen(name);
		ent->name = malloc(len + 1);
		strcpy(ent->name, name);
	} else
		ent->name = NULL;

	hyd_list_init(&ent->children);
	hyd_list_init(&ent->branch);
	hyd_list_init(&ent->properties);
	hyd_list_init(&ent->coll_objs);

	ent->pos.x = 0.0f;
	ent->pos.y = 0.0f;
	ent->parent = parent;
	if (parent != NULL) {
		hyd_list_append(&ent->branch, &ent->parent->children);
	}

	return ent;
}

void hyd_ent_draw(struct hyd_ent *entity, SDL_Renderer *renderer)
{
	if (entity == NULL)
		return;

	SDL_Point position;
	position.x = round(entity->pos.x);
	position.y = round(entity->pos.y);
	hyd_spr_draw_point(entity->spr, position, renderer);

	struct hyd_ent *iter;

	hyd_list_for_each_entry(iter, &entity->children, children)
	{
		hyd_ent_draw(iter, renderer);
	}

	struct hyd_coll_obj *col_iter;
	hyd_list_for_each_entry(col_iter, &entity->coll_objs, list)
	{
		hyd_coll_obj_draw(col_iter, renderer);
	}
}

void hyd_ent_destroy(struct hyd_ent *entity)
{
	if (entity == NULL)
		return;

	struct hyd_ent *child_iter;
	struct hyd_ent *child_next;
	hyd_list_for_each_entry_safe(child_iter, child_next, &entity->children, branch)
	{
		hyd_ent_destroy(child_iter);
	}

	struct hyd_property *p_iter;
	struct hyd_property *p_next;
	hyd_list_for_each_entry_safe(p_iter, p_next, &entity->properties, list)
	{
		hyd_property_destroy(p_iter);
	}

	struct hyd_coll_obj *col_iter;
	struct hyd_coll_obj *col_next;
	hyd_list_for_each_entry_safe(col_iter, col_next, &entity->coll_objs, list)
	{
		hyd_coll_obj_destroy(col_iter);
	}

	hyd_spr_destroy(entity->spr);
	free(entity->name);
	free(entity);
}

uint8_t hyd_ent_list_create_json(struct hyd_list *ent_list, json_t *root,
		struct hyd_list *textures, struct hyd_ent *parent, SDL_Renderer *renderer)
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
						textures, parent, renderer);

			hyd_list_append(&ent->branch, ent_list);
		} else if (json_is_object(obj_json)) {
			ent = hyd_ent_create_json(obj_json,
					textures, parent, renderer);

			hyd_list_append(&ent->branch, ent_list);
		}
		else
			continue;

		obj_json = json_object_get(arr_json, "x");
		if (json_is_number(obj_json))
			hyd_ent_set_position_x(ent, json_number_value(obj_json));

		obj_json = json_object_get(arr_json, "y");
		if (json_is_number(obj_json))
			hyd_ent_set_position_y(ent, json_number_value(obj_json));

		obj_json = json_object_get(arr_json, "sprite");
		if (json_is_string(obj_json)) {
			if (ent->spr != NULL)
				hyd_spr_destroy(ent->spr);
			ent->spr = hyd_spr_create_file(json_string_value(obj_json),
					textures, renderer);
		}

		obj_json = json_object_get(arr_json, "properties");
		if (json_is_object(obj_json))
			hyd_property_list_create_json(&ent->properties, obj_json);
	}

	return 0;
}

struct hyd_ent *hyd_ent_create_json(json_t *root, struct hyd_list *textures,
		struct hyd_ent *parent, SDL_Renderer *renderer)
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
	struct hyd_spr *sprite = NULL;

	iter_json = json_object_get(root, "name");
	if (json_is_string(iter_json))
		name = json_string_value(iter_json);

	iter_json = json_object_get(root, "sprite");
	if (json_is_string(iter_json))
		sprite = hyd_spr_create_file(json_string_value(iter_json),
				textures, renderer);

	ent = hyd_ent_create(sprite, name, parent);

	iter_json = json_object_get(root, "children");
	if (json_is_array(iter_json))
		hyd_ent_list_create_json(&ent->children,
				iter_json, textures, ent, renderer);

	iter_json = json_object_get(root, "properties");
	if (json_is_object(iter_json))
		hyd_property_list_create_json(&ent->properties, iter_json);

	iter_json = json_object_get(root, "collisions");
	if (json_is_array(iter_json))
		hyd_coll_obj_list_create_json(&ent->coll_objs, iter_json,
				&ent->pos);

	return ent;
}

struct hyd_ent *hyd_ent_create_file(const char *filename,
		struct hyd_list *textures, struct hyd_ent *parent, SDL_Renderer *renderer)
{
	uint8_t *buf = NULL;
	PHYSFS_sint64 read_length = 0;
	json_t *root_node = NULL;
	json_error_t error;
	struct hyd_ent *ent = NULL;

	read_length = hyd_fs_read_buffer(filename, &buf);
	if (read_length == 0) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"Got no data from '%s'",
				filename
				);
		return NULL;
	}

	SDL_Log("Reading object - %s - size: %i", filename, read_length);

	root_node = json_loadb(buf, read_length, 0, &error);
	free(buf);

	if (root_node == NULL) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"JSON error for file '%s' - line: %d - message: %s.",
				filename, error.line, error.text
				);
		return NULL;
	}

	ent = hyd_ent_create_json(root_node, textures, parent, renderer);
	json_decref(root_node);
	return ent;
}

struct hyd_ent *hyd_ent_list_find_first(struct hyd_list *entities, const char *name)
{
	struct hyd_ent *iter;
	hyd_list_for_each_entry(iter, entities, branch)
	{
		if (strcmp(iter->name, name) == 0)
			return iter;
	}

	return NULL;
}

float hyd_ent_get_number_property(struct hyd_ent *ent, const char *name)
{
	struct hyd_property *iter;
	hyd_list_for_each_entry(iter, &ent->properties, list)
	{
		if (strcmp(iter->name, name) == 0)
			return iter->value.n;
	}

	return 0.0f;
}

uint8_t hyd_ent_get_bool_property(struct hyd_ent *ent, const char *name)
{
	struct hyd_property *iter;
	hyd_list_for_each_entry(iter, &ent->properties, list)
	{
		if (strcmp(iter->name, name) == 0)
			return iter->value.b;
	}

	return 0;
}

const char *hyd_ent_get_string_property(struct hyd_ent *ent, const char *name)
{
	struct hyd_property *iter;
	hyd_list_for_each_entry(iter, &ent->properties, list)
	{
		if (strcmp(iter->name, name) == 0)
			return iter->value.s;
	}

	return "";
}

struct hyd_spr *hyd_ent_get_sprite(struct hyd_ent *entity)
{
	return entity->spr;
}

const char *hyd_ent_get_name(struct hyd_ent *entity)
{
	return entity->name;
}

struct hyd_ent *hyd_ent_get_parent(struct hyd_ent *entity)
{
	return entity->parent;
}

float hyd_ent_get_position_x(struct hyd_ent *entity)
{
	return entity->pos.x;
}

float hyd_ent_get_position_y(struct hyd_ent *entity)
{
	return entity->pos.y;
}

struct hyd_list *hyd_ent_get_coll_objs(struct hyd_ent *ent)
{
	return &ent->coll_objs;
}

void hyd_ent_set_position_x(struct hyd_ent *entity, float x)
{
	entity->pos.x = x;
}

void hyd_ent_set_position_y(struct hyd_ent *entity, float y)
{
	entity->pos.y = y;
}

void hyd_ent_set_number_property(struct hyd_ent *ent, float value, const char *name)
{
	struct hyd_property *iter;
	hyd_list_for_each_entry(iter, &ent->properties, list)
	{
		if (strcmp(iter->name, name) == 0) {
			iter->type = NUMBER;
			iter->value.n = value;
			return;
		}
	}

	hyd_list_append(&hyd_property_create_number(value, name)->list,
			&ent->properties);
}

void hyd_ent_set_bool_property(struct hyd_ent *ent, uint8_t value, const char *name)
{
	struct hyd_property *iter;
	hyd_list_for_each_entry(iter, &ent->properties, list)
	{
		if (strcmp(iter->name, name) == 0) {
			iter->type = BOOL;
			iter->value.b = value;
			return;
		}
	}

	hyd_list_append(&hyd_property_create_bool(value, name)->list,
			&ent->properties);
}

void hyd_ent_set_string_property(struct hyd_ent *ent, const char *value, const char *name)
{
	struct hyd_property *iter;
	hyd_list_for_each_entry(iter, &ent->properties, list)
	{
		if (strcmp(iter->name, name) == 0) {
			iter->type = STRING;
			free(iter->value.s);
			iter->value.s = malloc(strlen(value) + 1);
			strcpy(iter->value.s, value);
			return;
		}
	}

	hyd_list_append(&hyd_property_create_string(value, name)->list,
			&ent->properties);
}
