#include "frame.h"
#include <string.h>

struct hyd_frame *hyd_frame_create(const char *name, const SDL_Rect rect)
{
	struct hyd_frame* frame = malloc(sizeof(*frame));

	if (frame == NULL)
	{
		SDL_LogError(
			SDL_LOG_CATEGORY_SYSTEM,
			"Failed to allocate memory for frame\n"
			);
		return NULL;
	}
	
	frame->name = malloc(strlen(name) + 1);
	strcpy(frame->name, name);
	frame->rect = rect;

	return frame;
}

struct hyd_frame *hyd_frame_create_json(json_t *root)
{
	if (!json_is_object(root))
	{
		SDL_LogError(
			SDL_LOG_CATEGORY_APPLICATION,
			"Frame is invalid JSON type. Expected object.\n"
		);

		return NULL;
	}

	json_t *iter_json = NULL;
	const char* name;
	SDL_Rect rect;

	iter_json = json_object_get(root, "name");
	if (!json_is_string(iter_json))
	{
		SDL_LogError(
			SDL_LOG_CATEGORY_APPLICATION,
			"Could not find name for frame.\n"
		);

		return NULL;
	}

	name = json_string_value(iter_json);

	iter_json = json_object_get(root, "x");
	if (!json_is_integer(iter_json))
	{
		SDL_LogWarn(
				SDL_LOG_CATEGORY_APPLICATION,
				"Could not get x value for frame '%s'",
				name
				);
	}
	else
		rect.x = json_integer_value(iter_json);

	iter_json = json_object_get(root, "y");
	if (!json_is_integer(iter_json))
	{
		SDL_LogWarn(
				SDL_LOG_CATEGORY_APPLICATION,
				"Could not get y value for frame '%s'",
				name
				);
	}
	else
		rect.y = json_integer_value(iter_json);

	iter_json = json_object_get(root, "width");
	if (!json_is_integer(iter_json))
	{
		SDL_LogWarn(
				SDL_LOG_CATEGORY_APPLICATION,
				"Could not get width value for frame '%s'",
				name
				);
	}
	else
		rect.w = json_integer_value(iter_json);

	iter_json = json_object_get(root, "height");
	if (!json_is_integer(iter_json))
	{
		SDL_LogWarn(
				SDL_LOG_CATEGORY_APPLICATION,
				"Could not get height value for frame '%s'",
				name
				);
	}
	else
		rect.h = json_integer_value(iter_json);

	return hyd_frame_create(name, rect);
}

struct hyd_frame **hyd_frame_array_create_json(json_t *root, uint32_t *num)
{
	if (!json_is_array(root))
	{
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"Frames is not JSON array"
				);
		return NULL;
	}

	*num = json_array_size(root);
	if (*num == 0)
		return NULL;

	struct hyd_frame **frames = calloc(*num, sizeof(*frames));
	uint32_t i = 0;

	for (i = 0; i < *num; i++)
	{
		json_t *frame_node;

		frame_node = json_array_get(root, i);
		if (!json_is_object(frame_node))
		{
			SDL_LogWarn(
				SDL_LOG_CATEGORY_APPLICATION,
				"Frame is invalid JSON type. Expected object.\n"
			);
			continue;
		}

		frames[i] = hyd_frame_create_json(frame_node);
	}

	return frames;
}

struct hyd_frame *hyd_frame_array_find(struct hyd_frame **frames,
		uint32_t num, const char *name)
{
	uint32_t i;
	for(i = 0; i < num; i++)
	{
		if (strcmp(frames[i]->name, name) == 0)
			return frames[i];
	}

	return NULL;
}

void hyd_frame_destroy(struct hyd_frame *frame)
{
	free(frame->name);
	free(frame);
}
