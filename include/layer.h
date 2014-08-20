/**
 * \file layer.h
 */

#ifndef HYD_LAYER_H
#define HYD_LAYER_H

#include "entity.h"
#include "texture.h"

/**
 * \struct hyd_layer
 *
 * A layer contains entities
 */
struct hyd_layer {
	struct hyd_ent *ent_head;
	struct hyd_layer *next;
	struct hyd_layer *prev;
};

struct hyd_layer *hyd_layer_create_json(json_t *root);

void hyd_layer_destroy(struct hyd_layer *l);

void hyd_layer_draw(struct hyd_layer *l);

struct hyd_ent **hyd_layer_find_ent_list(struct hyd_layer *l, const char *n, uint32_t *num);

#endif

