/**
 * \file engine.h
 */

#ifndef HYD_ENGINE_H
#define HYD_ENGINE_H

#include "scene.h"
#include "mod.h"
#include "list.h"
#include "input.h"

/**
 * \struct hyd_engine
 */
struct hyd_engine {
	uint8_t running;

	SDL_Renderer *renderer;
	SDL_Window *window;

	struct hyd_scene *current_scene;
	struct hyd_mod *current_mod;
	struct hyd_input_preset *current_input_preset;
	struct hyd_list input_presets;
	struct hyd_list textures;
};

struct hyd_engine *hyd_engine_create(void);

uint8_t hyd_engine_init(struct hyd_engine *engine, const char *argv[]);

void hyd_engine_destroy(struct hyd_engine *engine);

void hyd_engine_events(struct hyd_engine *engine);

void hyd_engine_update(struct hyd_engine *engine, uint32_t dt);

void hyd_engine_draw(struct hyd_engine *engine);

uint8_t hyd_engine_run(struct hyd_engine *engine);

uint8_t hyd_engine_load_scene(struct hyd_engine *engine, const char *filename);

uint8_t hyd_engine_load_input_preset(struct hyd_engine *engine, const char *filename);

uint8_t hyd_engine_load_mod(struct hyd_engine *engine, const char *filename);

/* GETTERS */
struct hyd_scene *hyd_engine_get_current_scene(struct hyd_engine *engine);
struct hyd_input_preset *hyd_engine_get_current_input_preset(struct hyd_engine *engine);
SDL_Renderer *hyd_engine_get_renderer(struct hyd_engine *engine);

#endif
