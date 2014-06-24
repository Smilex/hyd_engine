#include "property.h"

#include <string.h>
#include <stdlib.h>

struct hyd_property *hyd_property_create_number(float v, const char *n)
{
	struct hyd_property *p = malloc(sizeof(*p));
	if (p == NULL)
		return NULL;

	p->name = malloc(strlen(n) + 1);
	strcpy(p->name, n);

	p->type = NUMBER;
	p->value.n = v;
	p->next = p;

	return p;
}

struct hyd_property *hyd_property_create_string(const char *v, const char *n)
{
	struct hyd_property *p = malloc(sizeof(*p));
	if (p == NULL)
		return NULL;

	p->name = malloc(strlen(n) + 1);
	strcpy(p->name, n);

	p->type = STRING;
	p->value.s = malloc(strlen(v) + 1);
	strcpy(p->value.s, v);

	return p;
}

struct hyd_property *hyd_property_create_bool(uint8_t v, const char *n)
{
	struct hyd_property *p = malloc(sizeof(*p));
	if (p == NULL)
		return NULL;

	p->name = malloc(strlen(n) + 1);
	strcpy(p->name, n);

	p->type = BOOL;
	p->value.b = v;

	return p;
}

void hyd_property_destroy(struct hyd_property *p)
{
	free(p->name);
	if (p->type == STRING)
		free(p->value.s);
	free(p);
}

uint8_t hyd_property_create_json(struct hyd_property *l, json_t *root)
{
	if (!json_is_object(root))
		return 1;

	const char *key;
	json_t *value;
	struct hyd_property *iter, *ck_iter;
	uint8_t found;

	json_object_foreach(root, key, value) {
		found = 0;
		if (json_is_number(value))
			iter = hyd_property_create_number(json_number_value(value), key);
		else if (json_is_boolean(value)) {
			if (json_is_true(value))
				iter = hyd_property_create_bool(1, key);
			else if (json_is_false(value))
				iter = hyd_property_create_bool(0, key);
		}
		else if (json_is_string(value))
			iter = hyd_property_create_string(json_string_value(value), key);

		if (iter != NULL) {
			for (ck_iter = l->next; ck_iter != l; ck_iter = ck_iter->next)
			{
				if (strcmp(ck_iter->name, key) == 0) {
					if (ck_iter->type == STRING)
						free(ck_iter->value.s);
					ck_iter->type = iter->type;
					ck_iter->value = iter->value;
					found = 1;
				}
			}
			if (!found) {
				iter->next = l->next;
				l->next = iter;
			}
		}
	}

	return 0;
}
