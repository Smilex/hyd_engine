#include "scene.h"
#include <physfs.h>
#include <stdint.h>
#include <stdlib.h>

struct hyd_scene *hyd_scene_create(void)
{
	struct hyd_scene *s = malloc(sizeof(*s));
	if (s == NULL) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"Failed to allocate memory for scene"
				);
		return NULL;
	}
	s->layer_head = malloc(sizeof(*s->layer_head));
	s->layer_head->next = s->layer_head;
	s->layer_head->prev = s->layer_head;

	return s;
}

struct hyd_scene *hyd_scene_create_file(const char *fname, struct hyd_tex_list *tex_l)
{
	uint8_t *buf = NULL;
	PHYSFS_sint64 read_len = 0;
	json_t *root = NULL;
	json_error_t error;
	struct hyd_scene *s = NULL;

	read_len = hyd_fs_read_buffer(fname, &buf);
	if (read_len == 0) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"Got no data from '%s'",
				fname
				);
		return NULL;
	}

	SDL_Log("Reading scene - %s - size: %i", fname, read_len);

	root = json_loadb(buf, read_len, 0, &error);
	free(buf);

	if (root == NULL) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"JSON error for file '%s' - line: %d - message: %s.",
				fname, error.line, error.text
				);
		return NULL;
	}

	s = hyd_scene_create_json(root, tex_l);
	json_decref(root);
	return s;
}

struct hyd_scene *hyd_scene_create_json(json_t *root, struct hyd_tex_list *tex_l)
{
	if (!json_is_object(root)) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"Invalid scene file format. Root is not object"
				);
		return NULL;
	}

	struct hyd_scene *s = hyd_scene_create();
	json_t *iter, *l_iter = json_object_get(root, "layers");
	uint32_t i;
	struct hyd_layer *layer;
	if (!json_is_array(l_iter)) {
		SDL_LogWarn(
				SDL_LOG_CATEGORY_APPLICATION,
				"No layers array in scene."
				);
	} else {
		json_array_foreach(l_iter, i, iter) {
			layer = hyd_layer_create_json(iter, tex_l);
			layer->next = s->layer_head->next;
			layer->prev = s->layer_head;
			s->layer_head->next = layer;
		}
	}

	return s;
}

void hyd_scene_destroy(struct hyd_scene *s)
{
	if (s == NULL)
		return;

	struct hyd_layer *i,*n;
	for (i = s->layer_head->next, n = i->next;
			i != s->layer_head;
			i = n, i = i->next)
	{
		hyd_layer_destroy(i);
	}

	free(s);
}

void hyd_scene_draw(struct hyd_scene *s)
{
	if (s == NULL)
		return;

	struct hyd_layer *i;
	for (i = s->layer_head->next; i != s->layer_head; i = i->next)
	{
		hyd_layer_draw(i);
	}
}

struct hyd_ent **hyd_scene_find_ent_list(struct hyd_scene *s, const char *n, uint32_t *num) {
	struct hyd_ent **ret = NULL, **temp;
	uint32_t nu = 0, j;
	struct hyd_layer *i;
	*num = 0;
	for (i = s->layer_head->next; i != s->layer_head; i = i->next) {
		temp = hyd_layer_find_ent_list(i, n, &nu);
		if (nu > 0) {
			ret = realloc(ret, sizeof(*ret) * (*num + nu));
			for (j = 0; j < nu; j++)
				ret[*num + j] = temp[j];

			*num += nu;
		}
	}

	return ret;
}
