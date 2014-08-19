#include "layer.h"
#include <jansson.h>
#include <stdlib.h>

struct hyd_layer *hyd_layer_create_json(json_t *root, struct hyd_tex_list *tex_l) {
	if (!json_is_object(root))
		return NULL;

	struct hyd_layer *layer = malloc(sizeof(*layer));
	if (layer == NULL)
		return NULL;

	layer->next = layer;
	layer->prev = layer;
	layer->ent_head = malloc(sizeof(*layer->ent_head));
	layer->ent_head->next = layer->ent_head;
	json_t *ent_json = json_object_get(root, "entities");
	if (!json_is_array(ent_json)) {
		SDL_LogWarn(
				SDL_LOG_CATEGORY_APPLICATION,
				"No entity array in layer."
				);
	} else {
		hyd_ent_create_json_arr(layer->ent_head,
				ent_json, tex_l, NULL, layer);
	}

	return layer;
}

void hyd_layer_destroy(struct hyd_layer *l) {
	if (l == NULL)
		return;

	struct hyd_ent *i,*n;
	for (i = l->ent_head->next, n = i->next;
			i != l->ent_head;
			i = n, n = i->next)
	{
		hyd_ent_destroy(i);
	}

	free(l->ent_head);
	free(l);
}

void hyd_layer_draw(struct hyd_layer *l) {
	if (l == NULL)
		return;

	struct hyd_ent *i;
	for (i = l->ent_head->next; i != l->ent_head; i = i->next)
	{
		hyd_ent_draw(i);
	}
}

struct hyd_ent **hyd_layer_find_ent_list(struct hyd_layer *l, const char *n, uint32_t *num) {
	return hyd_ent_list_find(l->ent_head, n, num);
}
