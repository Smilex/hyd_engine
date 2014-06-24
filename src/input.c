#include "input.h"

#include <string.h>
#include <jansson.h>
#include "filesystem.h"
#include <stdlib.h>
#include <stdio.h>

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
	input.value = 0;

	input.type = KEY;
	input.code = code;
	input.callback = NULL;

	return input;
}

struct hyd_input hyd_input_create_json(const char *a, json_t *root)
{
	struct hyd_input ret = {0,};
	if (!json_is_object(root))
		return ret;

	json_t *iter;
	const char *type;

	iter = json_object_get(root, "type");
	if (!json_is_string(iter))
		return ret;

	type = json_string_value(iter);

	if (strcmp(type, "key") == 0)
	{
		iter = json_object_get(root, "key");
		if (!json_is_string(iter))
			return ret;

		return hyd_input_create_key(a,
				SDL_GetScancodeFromName(json_string_value(iter)));
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
	if (!json_is_object(root))
		return NULL;

	const char *key;
	json_t *value;
	struct hyd_ip *p = hyd_ip_create(n);
	uint32_t i = 0;

	p->count = json_object_size(root);
	p->inputs = calloc(p->count, sizeof(*p->inputs));

	json_object_foreach(root, key, value)
	{
		p->inputs[i] = hyd_input_create_json(key, value);
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
			if (p->inputs[i].type == KEY && state[p->inputs[i].code])
			{
				return hyd_input_get_max_value();
			}
		}
	}

	return 0;
}

void hyd_ip_add_callback(struct hyd_ip *p, const char *a,
		hyd_input_callback callback)
{
	if (p == NULL && callback == NULL)
		return;

	uint32_t i;
	for (i = 0; i < p->count; i++)
	{
		if (strcmp(p->inputs[i].action, a) == 0)
			p->inputs[i].callback = callback;
	}
}

void hyd_ip_destroy(struct hyd_ip *p)
{
	struct hyd_ip *i, *n;
	uint32_t j;
	for (i = p->next, n = i->next;
			i != p;
			i = n, n = i->next) {
		for (j = 0; j < i->count; j++)
			free(i->inputs[j].action);
		free(i->name);
		free(i->inputs);
		free(i);
	}
}
