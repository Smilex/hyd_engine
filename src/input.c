#include "input.h"

#include <string.h>
#include <jansson.h>
#include "filesystem.h"
#include <stdlib.h>
#include <stdio.h>

SDL_GameController *_hyd_game_ctrls[50];
uint32_t _hyd_num_game_ctrls = 0;

uint16_t hyd_input_get_max_value(void)
{
	return 0x7FFF;
}

struct hyd_input hyd_input_create_key(const char *a, uint8_t code)
{
	struct hyd_input input;
	uint32_t len = strlen(a);
	input.action = malloc(len + 1);
	strcpy(input.action, a);

	input.type = KEY;
	input.value.code = code;
	input.neg_mod = 0;

	return input;
}

struct hyd_input hyd_input_create_axis(const char *a, SDL_GameControllerAxis axis, uint8_t neg_mod) {
	struct hyd_input input;
	uint32_t len = strlen(a);
	input.action = malloc(len + 1);
	strcpy(input.action, a);

	input.type = AXIS;
	input.value.axis = axis;
	input.neg_mod = neg_mod;

	return input;
}

struct hyd_input hyd_input_create_json(json_t *root)
{
	struct hyd_input ret = {0,};
	if (!json_is_object(root))
		return ret;

	json_t *iter;
	const char *type, *value, *name;

	iter = json_object_get(root, "name");
	if (!json_is_string(iter))
		return ret;

	name = json_string_value(iter);
	iter = json_object_get(root, "type");
	if (!json_is_string(iter))
		return ret;

	type = json_string_value(iter);

	if (strcmp(type, "key") == 0)
	{
		iter = json_object_get(root, "value");
		if (!json_is_string(iter))
			return ret;

		value = json_string_value(iter);
		return hyd_input_create_key(name,
				SDL_GetScancodeFromName(value));
	}
	else if (strcmp(type, "axis") == 0) {
		iter = json_object_get(root, "value");
		if (!json_is_string(iter))
			return ret;

		value = json_string_value(iter);
		iter = json_object_get(root, "mod");
		uint8_t neg_mod = 0;
		if (json_is_string(iter) && strcmp(json_string_value(iter), "negative") == 0)
			neg_mod = 1;

		return hyd_input_create_axis(name,
				SDL_GameControllerGetAxisFromString(value), neg_mod);
	}
}

struct hyd_ip *hyd_ip_create(const char *n)
{
	struct hyd_ip *p = malloc(sizeof(*p));
	if (p == NULL)
		return NULL;

	p->name = malloc(strlen(n) + 1);
	strcpy(p->name, n);
	p->next = p;
	p->prev = p;
	p->inputs = NULL;

	return p;
}

uint8_t hyd_ip_create_file(struct hyd_ip *l, const char *fname)
{
	struct hyd_ip *p = NULL;
	uint8_t* buf = NULL;
	PHYSFS_sint64 read_len = 0;
	json_t* root = NULL;
	json_error_t err;
	const char* p_key;
	json_t* p_val = NULL;

	read_len = hyd_fs_read_buffer(fname, &buf);

	if (read_len == 0)
	{
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"Failed to read keymap from file: '%s'\n",
				fname
				);
		return 1;
	}

	root = json_loadb(buf, read_len, 0, &err);
	free(buf);

	if (root == NULL)
	{
		SDL_LogError(
				SDL_LOG_CATEGORY_APPLICATION,
				"Failed to read keymap file as JSON: '%s'\n",
				err.text
				);
		return 1;
	}

	json_object_foreach(root, p_key, p_val)
	{
		p = hyd_ip_create_json(p_key, p_val);
		if (p != NULL) {
			p->next = l->next;
			l->next = p;
			p->prev = l;
		}
	}

	json_decref(root);
	return 0;
}

struct hyd_ip *hyd_ip_create_json(const char *n, json_t *root)
{
	if (!json_is_array(root))
		return NULL;

	json_t *value;
	struct hyd_ip *p = hyd_ip_create(n);
	uint32_t i = 0, ind;

	p->count = json_array_size(root);
	p->inputs = calloc(p->count, sizeof(*p->inputs));

	json_array_foreach(root, ind, value)
	{
		p->inputs[i] = hyd_input_create_json(value);
		i++;
	}

	return p;
}

uint16_t hyd_ip_get_value(struct hyd_ip *p,
		const char *a)
{
	if (p == NULL)
		return 0;

	const uint8_t* state = SDL_GetKeyboardState(NULL);
	uint32_t i;
	for (i = 0; i < p->count; i++)
	{
		if (strcmp(p->inputs[i].action, a) == 0)
		{
			if (p->inputs[i].type == KEY && state[p->inputs[i].value.code])
			{
				return hyd_input_get_max_value();
			}
			else if (_hyd_num_game_ctrls > 0 && p->inputs[i].type == AXIS) {
				int v = SDL_GameControllerGetAxis(_hyd_game_ctrls[0], p->inputs[i].value.axis);
				if (p->inputs[i].neg_mod) {
					if (v > 0)
						v = 0;
					else
						v *= -1;
				}
				else {
					if (v < 0)
						v = 0;
				}

				return v;
			}
		}
	}

	return 0;
}

void hyd_ip_destroy(struct hyd_ip *p)
{
	uint32_t j;
	for (j = 0; j < p->count; j++)
		free(p->inputs[j].action);
	free(p->name);
	free(p->inputs);
	free(p);
}

uint32_t hyd_input_load_controllers(void) {
	uint32_t i;
	SDL_GameController *ctrl;
	for (i = 0; i < SDL_NumJoysticks(); i++) {
		if (SDL_IsGameController(i)) {
			ctrl = SDL_GameControllerOpen(i);
			if (ctrl) {
				_hyd_game_ctrls[_hyd_num_game_ctrls] = ctrl;
				_hyd_num_game_ctrls++;
			}
		}
	}
}
