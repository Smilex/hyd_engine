/**
 * \file text.h
 */

#ifndef HYD_TEXT_H
#define HYD_TEXT_H

#include <SDL.h>
#include <jansson.h>
#include "texture.h"
#include "quad.h"
#include "stb_truetype.h"
#include "gl_core_3_3.h"
#include "color.h"

struct hyd_font {
	struct hyd_tex *tex;
};

/*
 * \struct hyd_text
 *
 * hyd_text is designed for drawing text
 */
struct hyd_text {
	uint8_t ready;
	char *name;
	char *text;
	struct hyd_font *font;
	struct hyd_quad *uvs;
	struct hyd_quad *pos;
};

/*
 * \struct hyd_locale
 *
 * hyd_locale is a list of texts
 * for a language
 */
struct hyd_locale {
	char *name;
	uint32_t num_texts;
	struct hyd_text **texts;
	struct hyd_locale *next;
	struct hyd_locale *children;
};

struct hyd_font *hyd_font_create_file(const char *fname);

struct hyd_text
*hyd_text_create(const char *str, const char *name);

/*
 * This doesn't draw on screen, but prepares
 * the text for drawing
 */
uint8_t hyd_text_render(struct hyd_text *t, struct hyd_font *font);

/*
 * Parses the file so that names are
 * in the form of 'parent.child'
 */
uint8_t hyd_locale_create_file(struct hyd_locale *l, const char *fname);

uint8_t hyd_locale_create_json(struct hyd_locale *l, json_t *root, const char *name);

struct hyd_locale *hyd_locale_find(struct hyd_locale *l, const char *name);

struct hyd_text *hyd_locale_find_text(struct hyd_locale *l, const char *name);

/*
 * Prepares all hyd_texts in locale
 */
uint8_t hyd_locale_render(struct hyd_locale *l, struct hyd_font *font);

uint8_t hyd_text_draw(struct hyd_text *text, float x, float y, struct hyd_color c);

uint8_t hyd_text_draw_str(	struct hyd_font *font,
							const char *str,
							float x, float y,
							struct hyd_color c);

void hyd_font_destroy(struct hyd_font *font);

void hyd_text_destroy(struct hyd_text *text);

#endif
