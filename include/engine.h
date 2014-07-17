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

	void (*call_update)(struct hyd_engine*,uint32_t);
	void (*call_draw)(struct hyd_engine*);

	struct hyd_scene *current_scene;
	struct hyd_mod *current_mod;
	struct hyd_ip *curr_ip;
	struct hyd_ip *ip_head;
	struct hyd_tex_list *tex_head;
	struct hyd_locale *locale_head;
};

struct hyd_engine *hyd_engine_create(void);

uint8_t hyd_engine_init(struct hyd_engine *engine, const char *argv[]);

void hyd_engine_destroy(struct hyd_engine *engine);

void hyd_engine_events(struct hyd_engine *engine);

void hyd_engine_update(struct hyd_engine *engine, uint32_t dt);

void hyd_engine_draw(struct hyd_engine *engine);

void hyd_engine_begin_draw(struct hyd_engine *e);

void hyd_engine_end_draw(struct hyd_engine *e);

uint8_t hyd_engine_run(struct hyd_engine *engine);

uint8_t hyd_engine_load_scene(struct hyd_engine *engine, const char *filename);

uint8_t hyd_engine_load_ip(struct hyd_engine *engine, const char *filename);

uint8_t hyd_engine_load_mod(struct hyd_engine *engine, const char *filename);

uint8_t hyd_engine_load_locale(struct hyd_engine *engine, const char *filename);

/* \brief Get time since engine was started, in milliseconds
 */
uint32_t hyd_engine_get_time(void);

void hyd_engine_update_func(struct hyd_engine *e,
		void (*f)(struct hyd_engine*,uint32_t));

void hyd_engine_draw_func(struct hyd_engine *e,
		void (*f)(struct hyd_engine*));

void hyd_engine_set_tex_shdr(struct hyd_engine *e, struct hyd_program *s);

void hyd_engine_set_argb_shdr(struct hyd_engine *e, struct hyd_program *s);

struct hyd_program *hyd_tex_shdr(void);

struct hyd_program *hyd_argb_shdr(void);

struct hyd_program *hyd_gray_shdr(void);

#endif
