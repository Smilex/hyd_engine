#include "scene.h"
#include <physfs.h>
#include <stdint.h>
#include <stdlib.h>

struct hyd_scene *hyd_scene_create(void)
{
	struct hyd_scene *scene = malloc(sizeof(*scene));
	if (scene == NULL) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"Failed to allocate memory for scene"
				);
		return NULL;
	}

	hyd_list_init(&scene->entities);
	hyd_list_init(&scene->sprites);

	return scene;
}

struct hyd_scene *hyd_scene_create_file(const char *filename, struct hyd_list *textures,
		SDL_Renderer *renderer)
{
	uint8_t *buf = NULL;
	PHYSFS_sint64 read_length = 0;
	json_t *root_node = NULL;
	json_error_t error;
	struct hyd_scene *scene = NULL;

	read_length = hyd_fs_read_buffer(filename, &buf);
	if (read_length == 0) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"Got no data from '%s'",
				filename
				);
		return NULL;
	}

	SDL_Log("Reading scene - %s - size: %i", filename, read_length);

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

	scene = hyd_scene_create_json(root_node, textures, renderer);
	json_decref(root_node);
	return scene;
}

struct hyd_scene *hyd_scene_create_json(json_t *root, struct hyd_list *textures,
		SDL_Renderer *renderer)
{
	if (!json_is_object(root)) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"Invalid scene file format. Root is not object"
				);
		return NULL;
	}

	struct hyd_scene *scene = hyd_scene_create();

	json_t *ent_json = json_object_get(root, "entities");
	if (!json_is_array(ent_json)) {
		SDL_LogWarn(
				SDL_LOG_CATEGORY_APPLICATION,
				"No objects array in scene file."
				);
	} else {
		hyd_ent_list_create_json(&scene->entities,
				ent_json, textures, NULL, renderer);
	}

	return scene;
}

void hyd_scene_destroy(struct hyd_scene *scene)
{
	if (scene == NULL)
		return;

	SDL_Log("Destroying scene");

	struct hyd_ent *iter;
	struct hyd_ent *next;
	hyd_list_for_each_entry_safe(iter, next, &scene->entities, branch)
	{
		hyd_ent_destroy(iter);
	}

	free(scene);
}

void hyd_scene_draw(struct hyd_scene *scene, SDL_Renderer *renderer)
{
	if (scene == NULL)
		return;

	struct hyd_ent *iter;
	hyd_list_for_each_entry(iter, &scene->entities, branch)
	{
		hyd_ent_draw(iter, renderer);
	}
}

struct hyd_list *hyd_scene_get_entities(struct hyd_scene *scene)
{
	return &scene->entities;
}
