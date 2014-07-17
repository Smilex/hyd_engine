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
	s->ent_head = malloc(sizeof(*s->ent_head));
	s->ent_head->next = s->ent_head;

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

	json_t *ent_json = json_object_get(root, "entities");
	if (!json_is_array(ent_json)) {
		SDL_LogWarn(
				SDL_LOG_CATEGORY_APPLICATION,
				"No objects array in scene file."
				);
	} else {
		hyd_ent_create_json_arr(s->ent_head,
				ent_json, tex_l, NULL);
	}

	return s;
}

void hyd_scene_destroy(struct hyd_scene *s)
{
	if (s == NULL)
		return;

	SDL_Log("Destroying scene");

	struct hyd_ent *i,*n;
	for (i = s->ent_head->next, n = i->next;
			i != s->ent_head;
			i = n, n = i->next)
	{
		hyd_ent_destroy(i);
	}

	free(s);
}

void hyd_scene_draw(struct hyd_scene *s)
{
	if (s == NULL)
		return;

	struct hyd_ent *i;
	for (i = s->ent_head->next; i != s->ent_head; i = i->next)
	{
		hyd_ent_draw(i);
	}
}
