#include "sprite.h"
#include <physfs.h>
#include <SDL.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

struct hyd_spr *hyd_spr_create(struct hyd_tex *tex, struct hyd_frame **frames,
		uint32_t num_frames, struct hyd_anim **anims,
		uint32_t num_anims)
{
	struct hyd_spr *spr = malloc(sizeof(*spr));
	if (spr == NULL) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"Failed to allocate data for sprite"
				);
		return NULL;
	}

	spr->tex = tex;
	spr->frames = frames;
	spr->anims = anims;
	spr->num_frames = num_frames;
	spr->num_anims = num_anims;

	return spr;
}

struct hyd_spr *hyd_spr_create_json(json_t *root, struct hyd_list *textures,
		SDL_Renderer *renderer)
{
	uint32_t i = 0;
	struct hyd_tex *texture = NULL;
	struct hyd_frame **frames;
	struct hyd_anim **anims;
	uint32_t num_frames;
	uint32_t num_anims;
	json_t *iter_json;

	if (!json_is_object(root)) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"Invalid sprite file format\n"
				);
		return NULL;
	}

	iter_json = json_object_get(root, "img");
	if (!json_is_string(iter_json)) {
		SDL_LogError(
			SDL_LOG_CATEGORY_APPLICATION,
			"Could not find 'img' value\n"
			);
		return NULL;
	}

	texture = hyd_tex_list_find(textures, json_string_value(iter_json));
	if (texture == NULL) {
		texture = hyd_tex_create_file(json_string_value(iter_json), renderer);
		hyd_list_append(&texture->list, textures);
	}

	iter_json = json_object_get(root, "frames");
	if (!json_is_array(iter_json)) {
		SDL_LogError(
			SDL_LOG_CATEGORY_APPLICATION,
			"Could not find 'frames' array\n"
			);
		return NULL;
	}

	frames = hyd_frame_array_create_json(iter_json, &num_frames);

	iter_json = json_object_get(root, "animations");
	if (json_is_array(iter_json))
		anims = hyd_anim_array_create_json(iter_json,
				frames, num_frames, &num_anims);

	json_decref(root);
	return hyd_spr_create(hyd_tex_copy(texture),
			frames, num_frames, anims, num_anims);
}

struct hyd_spr *hyd_spr_create_file(const char *filename, struct hyd_list *textures,
		SDL_Renderer *renderer)
{
	PHYSFS_sint64 file_length = 0;
	uint8_t *buf = NULL;
	json_t *root_node = NULL;
	json_error_t error;

	file_length = hyd_fs_read_buffer(filename, &buf);
	if (file_length == 0) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"Failed to read sprite from file: '%s'\n",
				filename
				);
		return NULL;
	}

	SDL_Log("Reading sprite - %s - size: %i", filename, file_length);

	root_node = json_loadb(buf, file_length, 0, &error);
	free(buf);
	buf = NULL;

	if (root_node == NULL) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"JSON error for file '%s' - line: %d - message: %s.",
				filename, error.line, error.text
				);
		return NULL;
	}

	return hyd_spr_create_json(root_node, textures, renderer);
}

void hyd_spr_destroy(struct hyd_spr *sprite)
{
	if (sprite == NULL)
		return;

	SDL_Log("Destroying sprite");
	uint32_t i;
	for (i = 0; i < sprite->num_anims; i++)
		hyd_anim_destroy(sprite->anims[i]);

	free(sprite->anims);

	for (i = 0; i < sprite->num_frames; i++)
		hyd_frame_destroy(sprite->frames[i]);

	free(sprite->frames);

	hyd_tex_destroy(sprite->tex);
	free(sprite);
}

void hyd_spr_list_destroy(struct hyd_list *sprites)
{
	if (sprites == NULL)
		return;

	struct hyd_spr *iter;
	struct hyd_spr *next;
	hyd_list_for_each_entry_safe(iter, next, sprites, list)
	{
		hyd_spr_destroy(iter);
	}
}

void hyd_spr_draw_point(struct hyd_spr *sprite, SDL_Point point,
		SDL_Renderer *renderer)
{
	if (sprite == NULL)
		return;

	SDL_Rect rect;
	rect.x = point.x;
	rect.y = point.y;
	rect.w = sprite->frames[0]->rect.w;
	rect.h = sprite->frames[0]->rect.h;

	if (sprite->tex == NULL) {
		SDL_SetRenderDrawColor(renderer, 255, 105, 180, 255);
		SDL_RenderFillRect(renderer, &rect);
		SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
		return;
	}

	SDL_Texture *tex = sprite->tex->ptr;
	SDL_RenderCopy(renderer, tex, &(sprite->frames[0]->rect), &rect);
}
