#include "text.h"
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#include "filesystem.h"
#include "texture.h"
#include "quad.h"
#include "color.h"
#define STB_TRUETYPE_IMPLEMENTATION
#include "stb_truetype.h"

stbtt_bakedchar cdata[96];

struct hyd_font *hyd_font_create_file(const char *fname) {
	uint8_t *buf;
	PHYSFS_sint64 read_len;
	struct hyd_font *font;

	read_len = hyd_fs_read_buffer(fname, &buf);
	if (read_len == 0)
		return NULL;

	font = malloc(sizeof(*font));
	unsigned char temp_bitmap[512 * 512];
	stbtt_BakeFontBitmap(buf,0, 32.0, temp_bitmap,512,512, 32,96, cdata);
	free(buf);

	font->tex = hyd_tex_create("font");
	glBindTexture(GL_TEXTURE_2D, font->tex->ptr);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, 512, 512, 0, GL_RED, GL_UNSIGNED_BYTE, temp_bitmap);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glBindTexture(GL_TEXTURE_2D, 0);

	return font;
}

struct hyd_text
*hyd_text_create(const char *str, const char *name) {
	struct hyd_text *text = malloc(sizeof(*text));
	if (text == NULL)
		return NULL;

	text->font = NULL;
	text->ready = 0;

	text->text = malloc(strlen(str) + 1);
	if (text->text == NULL) {
		free(text);
		return NULL;
	}

	strcpy(text->text, str);
	text->uvs = NULL;

	text->name = malloc(strlen(name) + 1);
	if (text->name == NULL) {
		free(text->text);
		free(text);
		return NULL;
	}

	strcpy(text->name, name);

	return text;
}

uint8_t hyd_text_render(struct hyd_text *t, struct hyd_font *font) {
	t->uvs = calloc(strlen(t->text), sizeof(*t->uvs));
	t->pos = calloc(strlen(t->text), sizeof(*t->pos));
	t->font = font;

	char const *str = t->text;
	float x = 0, y = 0;
	uint32_t i = 0;
	while (*str) {
		if (*str >= 32 && *str < 128) {
			stbtt_aligned_quad q;
			stbtt_GetBakedQuad(cdata, 512,512, *str-32, &x, &y, &q, 1);
			t->pos[i].x1 = q.x0;
			t->pos[i].y1 = q.y0;
			t->pos[i].x2 = q.x1;
			t->pos[i].y2 = q.y1;

			t->uvs[i].x1 = q.s0;
			t->uvs[i].y1 = q.t1;
			t->uvs[i].x2 = q.s1;
			t->uvs[i].y2 = q.t0;
		}
		++i;
		++str;
	}

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

uint8_t hyd_locale_render(struct hyd_locale *l, struct hyd_font *font) {
	uint32_t i;
	uint8_t r = 0;

	for (i = 0; i < l->num_texts; i++) {
		if (hyd_text_render(l->texts[i], font) != 0)
			r = 1;
	}

	return r;
}

uint8_t hyd_text_draw(struct hyd_text *text, float x, float y, struct hyd_color c)
{
	if (!text->ready)
		return 1;

	uint32_t i, n = strlen(text->text);
	for (i = 0; i < n; i++) {
		struct hyd_quad p = text->pos[i];
		p.x1 += x;
		p.y1 += y;
		p.x2 += x;
		p.y2 += y;
		hyd_quad_tex_draw(&p, &c, text->font->tex, &text->uvs[i]);
	}

	return 0;
}

uint8_t hyd_text_draw_str(	struct hyd_font *font,
							const char *str,
							float x, float y,
							struct hyd_color c)
{
	while (*str) {
		if (*str >= 32 && *str < 128) {
			stbtt_aligned_quad q;
			stbtt_GetBakedQuad(cdata, 512,512, *str-32, &x, &y, &q, 1);
			struct hyd_quad p = {
				q.x0, q.y0, q.x1, q.y1
			};
			struct hyd_quad uv = {
				q.s0, q.t1, q.s1, q.t0
			};
			
			hyd_quad_tex_draw(&p, &c, font->tex, &uv);
		}
		++str;
	}
	return 0;
}

void hyd_font_destroy(struct hyd_font *font) {
	hyd_tex_destroy(font->tex);
	free(font);
}

void hyd_text_destroy(struct hyd_text *text) {
	free(text->text);
	free(text->name);
	free(text->uvs);
	free(text->pos);
	free(text);
}
