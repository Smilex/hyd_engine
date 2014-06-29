#include "text.h"
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#include "filesystem.h"
#include "geom.h"

TTF_Font *hyd_font_create_file(const char *fname) {
	uint8_t *buf;
	PHYSFS_sint64 read_len;
	TTF_Font *font;

	read_len = hyd_fs_read_buffer(fname, &buf);
	if (read_len == 0)
		return NULL;

	SDL_RWops *ops = SDL_RWFromMem(buf, read_len);
	if (ops == NULL)
		return NULL;

	font = TTF_OpenFontRW(ops, 1, 16);

	return font;
}

struct hyd_text
*hyd_text_create(const char *str, const char *name) {
	struct hyd_text *text = malloc(sizeof(*text));
	if (text == NULL)
		return NULL;

	text->tex = NULL;
	text->font = NULL;
	text->src.x = 0;
	text->src.y = 0;
	text->src.w = 0;
	text->src.h = 0;
	text->ready = 0;

	text->text = malloc(strlen(str) + 1);
	if (text->text == NULL) {
		free(text);
		return NULL;
	}

	strcpy(text->text, str);

	text->name = malloc(strlen(name) + 1);
	if (text->name == NULL) {
		free(text->text);
		free(text);
		return NULL;
	}

	strcpy(text->name, name);

	return text;
}

uint8_t hyd_text_render(struct hyd_text *t, SDL_Renderer *rend, TTF_Font *font, uint32_t hex) {
	hyd_hex_color(hex, &t->color.r, &t->color.g, &t->color.b);
	t->color.a = 255;

	t->font = font;
	SDL_Surface *surf = TTF_RenderUTF8_Solid(t->font, t->text, t->color);
	if (surf == NULL)
		return 1;

	t->tex = SDL_CreateTextureFromSurface(rend, surf);
	SDL_FreeSurface(surf);

	if (t->tex == NULL)
		return 1;

	if (TTF_SizeUTF8(t->font, t->text, &t->src.w, &t->src.h) == -1)
		return 1;

	t->ready = 1;
	return 0;
}

uint8_t hyd_locale_create_file(struct hyd_locale *l, const char *fname) {
	uint8_t *buf, r;
	PHYSFS_sint64 read_len;
	json_t *root, *val;
	const char *key;
	json_error_t err;

	read_len = hyd_fs_read_buffer(fname, &buf);
	if (read_len == 0) {
		free(buf);
		return 1;
	}

	root = json_loadb(buf, read_len, 0, &err);
	free(buf);

	if (root == NULL)
		return 1;

	json_object_foreach(root, key, val) {
		hyd_locale_create_json(l, val, key);
	}
	json_decref(root);

	return 0;
}

uint8_t hyd_locale_create_json(struct hyd_locale *l, json_t *root, const char *name) {
	if (!json_is_object(root) || json_object_size(root) <= 0)
		return 1;

	struct hyd_locale *n;
	const char *key;
	json_t* val;
	n = malloc(sizeof(*n));
	if (n == NULL)
		return 1;
	n->next = n;
	n->children = malloc(sizeof(*n->children));
	if (n->children == NULL) {
		free(n);
		return 1;
	}
	n->children->next = n->children;
	n->num_texts = 0;
	n->texts = calloc(json_object_size(root), sizeof(*n->texts));
	if (n->texts == NULL) {
		free(n->children);
		free(n);
		return 1;
	}
	n->name = malloc(strlen(name) + 1);
	if (n->name == NULL) {
		free(n->texts);
		free(n);
		return 1;
	}
	strcpy(n->name, name);

	json_object_foreach(root, key, val) {
		if (json_is_object(val)) {
			if (hyd_locale_create_json(n->children, val, key) != 0)
				continue;
		}
		else if (json_is_string(val)) {
			struct hyd_text *t = hyd_text_create(json_string_value(val), key);
			if (t == NULL)
				continue;
			n->texts[n->num_texts] = t;
			n->num_texts++;
		}
	}

	if (n->num_texts == 0)
		free(n->texts);
	else {
		n->texts = realloc(n->texts, n->num_texts * sizeof(*n->texts));
		if (n->texts == NULL)
			return 1;
	}
	
	n->next = l->next;
	l->next = n;

	return 0;
}

struct hyd_locale *hyd_locale_find(struct hyd_locale *l, const char *name) {
	struct hyd_locale *i;
	for (i = l->next; i != l; i = i->next) {
		if (strcmp(i->name, name) == 0)
			return i;
	}

	return NULL;
}

struct hyd_text *hyd_locale_find_text(struct hyd_locale *l, const char *name) {
	uint32_t i;
	for (i = 0; i < l->num_texts; i++) {
		if (strcmp(l->texts[i]->name, name) == 0)
			return l->texts[i];
	}

	return NULL;
}

uint8_t hyd_locale_render(struct hyd_locale *l, SDL_Renderer *rend, TTF_Font *font, uint32_t hex) {
	uint32_t i;
	uint8_t r = 0;

	for (i = 0; i < l->num_texts; i++) {
		if (hyd_text_render(l->texts[i], rend, font, hex) != 0)
			r = 1;
	}

	return r;
}

uint8_t hyd_text_draw(	struct SDL_Renderer *rend,
						struct hyd_text *text,
						SDL_Point pos)
{
	if (!text->ready)
		return 1;

	SDL_Rect dest = text->src;
	dest.x = pos.x;
	dest.y = pos.y;

	SDL_RenderCopy(rend, text->tex, &text->src, &dest);

	return 0;
}

uint8_t hyd_text_draw_str(	struct SDL_Renderer *rend,
							TTF_Font *font,
							const char *str,
							SDL_Point pos,
							uint32_t hex)
{
	SDL_Color color;

	hyd_hex_color(hex, &color.r, &color.g, &color.b);

	SDL_Surface *surf = TTF_RenderUTF8_Solid(font, str, color);
	SDL_Texture *tex;
	SDL_Rect src, dest;
	src.x = 0;
	src.y = 0;
	dest.x = pos.x;
	dest.y = pos.y;

	if (surf == NULL)
		return 1;

	tex = SDL_CreateTextureFromSurface(rend, surf);
	SDL_FreeSurface(surf);

	if (tex == NULL)
		return 1;

	if (TTF_SizeUTF8(font, str, &src.w, &src.h) == -1)
		return 1;

	dest.w = src.w;
	dest.h = src.h;

	SDL_RenderCopy(rend, tex, &src, &dest);

	return 0;
}

void hyd_font_destroy(TTF_Font *font) {
	TTF_CloseFont(font);
}

void hyd_text_destroy(struct hyd_text *text) {
	free(text->text);
	free(text->tex);
	free(text);
}
