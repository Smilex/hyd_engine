/**
 * \file text.h
 */

#ifndef HYD_TEXT_H
#define HYD_TEXT_H

#include <SDL.h>
#include <SDL_ttf.h>
#include <jansson.h>

/*
 * \struct hyd_text
 *
 * hyd_text is designed for drawing text
 */
struct hyd_text {
	uint8_t ready;
	char *name;
	char *text;
	SDL_Color color;
	SDL_Texture *tex;
	TTF_Font *font;
	SDL_Rect src;
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

TTF_Font *hyd_font_create_file(const char *fname);

struct hyd_text
*hyd_text_create(const char *str, const char *name);

/*
 * This doesn't draw on screen, but prepares
 * the text for drawing
 */
uint8_t hyd_text_render(struct hyd_text *t, SDL_Renderer *rend, TTF_Font *font, uint32_t hex);

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
uint8_t hyd_locale_render(struct hyd_locale *l, SDL_Renderer *rend, TTF_Font *font, uint32_t hex);

uint8_t hyd_text_draw(	struct SDL_Renderer *rend,
						struct hyd_text *text,
						SDL_Point pos);

uint8_t hyd_text_draw_str(	struct SDL_Renderer *rend,
							TTF_Font *font,
							const char *str,
							SDL_Point pos,
							uint32_t hex);

void hyd_font_destroy(TTF_Font *font);

void hyd_text_destroy(struct hyd_text *text);

#endif
