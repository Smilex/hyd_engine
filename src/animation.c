#include "animation.h"

#include <stdlib.h>
#include <string.h>
#include <SDL.h>
#include "engine.h"

struct hyd_anim *hyd_anim_create(const char *name, struct hyd_frame **frames,
		uint32_t num_frames)
{
	struct hyd_anim *anim = malloc(sizeof(*anim));
	if (anim == NULL)
	{
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"Failed to allocate memory for animation."
				);
		return NULL;
	}

	anim->name = malloc(strlen(name) + 1);
	strcpy(anim->name, name);

	anim->frames = frames;
	anim->num_frames = num_frames;
	anim->repeat = 0;
	anim->curr_frame = 0;
	anim->delay = 16;
	anim->last_time = anim->delay;
	anim->start_frame = 0;

	return anim;
}

struct hyd_anim *hyd_anim_create_json(json_t *root, struct hyd_frame **frames,
		uint32_t num_frames)
{
	if (!json_is_object(root))
	{
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"JSON for animation is not an object."
				);
		return NULL;
	}

	json_t *iter_json;
	const char *name;
	struct hyd_frame **sel_frames; // frames that the animation will use
	uint32_t num_sel_frames;
	uint32_t i;

	iter_json = json_object_get(root, "name");
	if (!json_is_string(iter_json))
	{
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"'name' entry of animation has no string value."
				);
		return NULL;
	}
	name = json_string_value(iter_json);

	iter_json = json_object_get(root, "frames");
	if (!json_is_array(iter_json))
	{
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"'frames' entry of animation has no array value."
				);
		return NULL;
	}

	num_sel_frames = json_array_size(iter_json);
	sel_frames = calloc(num_sel_frames, sizeof(*sel_frames));
	for (i = 0; i < num_sel_frames; i++)
	{
		json_t *name_json;
		name_json = json_array_get(iter_json, i);

		sel_frames[i] = hyd_frame_array_find(frames, num_frames,
				json_string_value(name_json));
	}

	struct hyd_anim *ret = hyd_anim_create(name, sel_frames, num_sel_frames);

	iter_json = json_object_get(root, "repeat");
	if (json_is_true(iter_json))
		ret->repeat = 1;

	iter_json = json_object_get(root, "delay");
	if (json_is_number(iter_json))
		ret->delay = (uint32_t)json_number_value(iter_json);

	iter_json = json_object_get(root, "start");
	if (json_is_string(iter_json)) {
		for (i = 0; i < ret->num_frames; i++) {
			if (strcmp(ret->frames[i]->name, json_string_value(iter_json)) == 0) {
				ret->curr_frame = i;
				ret->start_frame = i;
				break;
			}
		}
	}


	return ret;
}

struct hyd_anim **hyd_anim_array_create_json(json_t *root, struct hyd_frame **frames,
		uint32_t num_frames, uint32_t *num)
{
	if (!json_is_array(root))
	{
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"JSON for animation array is not an array."
				);
		return NULL;
	}

	*num = json_array_size(root);
	if (*num == 0)
		return NULL;

	struct hyd_anim **animations = calloc(*num, sizeof(**animations));
	uint32_t i;

	for (i = 0; i < *num; i++)
	{
		json_t *anim_node;

		anim_node = json_array_get(root, i);
		if (!json_is_object(anim_node))
		{
			SDL_LogWarn(
				SDL_LOG_CATEGORY_APPLICATION,
				"Animation is invalid JSON type. Expected object.\n"
			);
			continue;
		}

		animations[i] = hyd_anim_create_json(anim_node, frames, num_frames);
	}

	return animations;
}

void hyd_anim_destroy(struct hyd_anim *animation)
{
	free(animation->frames);
	free(animation->name);
	free(animation);
}

struct hyd_frame *hyd_anim_get_next(struct hyd_anim *anim) {
	uint32_t time = hyd_engine_get_time();
	if (time >= anim->last_time + anim->delay) {
		anim->last_time = time;
		if (anim->curr_frame < anim->num_frames)
			anim->curr_frame++;
		if (anim->curr_frame == anim->num_frames)
			if(anim->repeat)
				anim->curr_frame = 0;
			else
				anim->curr_frame--;
	}

	return anim->frames[anim->curr_frame];
}

struct hyd_frame *hyd_anim_get_prev(struct hyd_anim *anim) {
	uint32_t time = hyd_engine_get_time();
	if (time >= anim->last_time + anim->delay) {
		anim->last_time = time;
		if (anim->curr_frame > 0)
			anim->curr_frame--;
		if (anim->curr_frame == 0)
			if(anim->repeat)
				anim->curr_frame = anim->num_frames - 1;
	}

	return anim->frames[anim->curr_frame];
}

struct hyd_anim *hyd_anim_array_find(struct hyd_anim **anims,
		uint32_t num, const char *name) {
	uint32_t i;
	for(i = 0; i < num; i++)
	{
		if (strcmp(anims[i]->name, name) == 0)
			return anims[i];
	}

	return NULL;
}

