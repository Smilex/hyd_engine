#include "sprite.h"
#include <physfs.h>
#include <SDL.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include "transform.h"

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

struct hyd_spr *hyd_spr_create_json(json_t *root, struct hyd_tex_list *tex_l)
{
	uint32_t i = 0;
	struct hyd_tex *tex = NULL;
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

	tex = hyd_tex_list_find(tex_l, json_string_value(iter_json));
	if (tex == NULL) {
		tex = hyd_tex_create_file(json_string_value(iter_json));
		struct hyd_tex_list *l = malloc(sizeof(*l));
		l->tex = tex;
		l->next = tex_l->next;
		tex_l->next = l;
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
	return hyd_spr_create(hyd_tex_copy(tex),
			frames, num_frames, anims, num_anims);
}

struct hyd_spr *hyd_spr_create_file(const char *fname, struct hyd_tex_list *tex_l)
{
	PHYSFS_sint64 file_len = 0;
	uint8_t *buf = NULL;
	json_t *root = NULL;
	json_error_t err;

	file_len = hyd_fs_read_buffer(fname, &buf);
	if (file_len == 0) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"Failed to read sprite from file: '%s'\n",
				fname
				);
		return NULL;
	}

	SDL_Log("Reading sprite - %s - size: %i", fname, file_len);

	root = json_loadb(buf, file_len, 0, &err);
	free(buf);

	if (root == NULL) {
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"JSON error for file '%s' - line: %d - message: %s.",
				fname, err.line, err.text
				);
		return NULL;
	}

	return hyd_spr_create_json(root, tex_l);
}

void hyd_spr_destroy(struct hyd_spr *s)
{
	if (s == NULL)
		return;

	SDL_Log("Destroying sprite");
	uint32_t i;
	for (i = 0; i < s->num_anims; i++)
		hyd_anim_destroy(s->anims[i]);

	free(s->anims);

	for (i = 0; i < s->num_frames; i++)
		hyd_frame_destroy(s->frames[i]);

	free(s->frames);

	hyd_tex_destroy(s->tex);
	free(s);
}

void hyd_spr_draw_point(struct hyd_spr *spr, SDL_Point point)
{
	if (spr == NULL)
		return;

	SDL_Rect rect;
	rect.x = point.x;
	rect.y = point.y;
	rect.w = spr->frames[0]->rect.w;
	rect.h = spr->frames[0]->rect.h;

	struct hyd_quad quad = {
		point.x, point.y,
		point.x + spr->frames[0]->rect.w,
		point.y + spr->frames[0]->rect.h
	};

	struct hyd_color col = {
		1.0f, 1.0f, 1.0f, 1.0f
	};
	
	struct hyd_quad uv = {
		((float)spr->frames[0]->rect.x) / ((float)spr->tex->w),
		((float)spr->frames[0]->rect.y) / ((float)spr->tex->h),
		((float)spr->frames[0]->rect.x + spr->frames[0]->rect.w) / ((float)spr->tex->w),
		((float)spr->frames[0]->rect.y + spr->frames[0]->rect.h) / ((float)spr->tex->h)
	};

	hyd_quad_tex_draw(&quad, &col, spr->tex, &uv);
}
