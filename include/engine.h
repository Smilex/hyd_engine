/**
 * \file engine.h
 */

#ifndef HYD_ENGINE_H
#define HYD_ENGINE_H

#include <SDL.h>

#include "scene.h"
#include "mod.h"
#include "input.h"
#include "texture.h"
#include "text.h"
#include "graphics.h"
#include "transform.h"

/**
 * \struct hyd_engine
 */
struct hyd_engine {
	uint8_t running;
	uint8_t quit;
	uint8_t pause;

	SDL_GLContext context;
	struct SDL_Window *window;

	struct hyd_scene *current_scene;
	struct hyd_mod *current_mod;
	struct hyd_ip *curr_ip;
	struct hyd_ip *ip_head;
	struct hyd_tex_list *tex_head;
	struct hyd_locale *locale_head;
};

struct hyd_engine *hyd_engine_get(void);

uint8_t hyd_engine_init(const char *argv[], uint32_t w, uint32_t h);

void hyd_engine_destroy(void);

void hyd_engine_events(void);

void hyd_engine_update(uint32_t dt);

void hyd_engine_draw(void);

void hyd_engine_begin_draw(void);

void hyd_engine_end_draw(void);

uint8_t hyd_engine_run(void);

uint8_t hyd_engine_load_scene(const char *filename);

uint8_t hyd_engine_load_ip(const char *filename);

uint8_t hyd_engine_load_locale(const char *filename);

/* \brief Get time since engine was started, in milliseconds
 */
uint32_t hyd_engine_get_time(void);

void hyd_engine_set_tex_shdr(struct hyd_program *s);

void hyd_engine_set_argb_shdr(struct hyd_program *s);

struct hyd_program *hyd_tex_shdr(void);

struct hyd_program *hyd_argb_shdr(void);

struct hyd_program *hyd_gray_shdr(void);

struct hyd_scene *hyd_engine_get_scene(void);

struct hyd_ip *hyd_engine_get_ip(void);

struct hyd_locale *hyd_engine_get_locale(void);

struct hyd_tex_list *hyd_engine_get_tex_list(void);

uint8_t hyd_engine_get_quit();

#endif
