#include "engine.h"
#include "init.h"

struct hyd_engine *hyd_engine_create(void)
{
	struct hyd_engine *engine = malloc(sizeof(*engine));
	if (engine == NULL)
		return NULL;

	engine->running = 1;
	engine->current_scene = NULL;
	engine->current_input_preset = NULL;
	engine->current_mod = NULL;
	engine->renderer = NULL;
	engine->window = NULL;

	hyd_list_init(&engine->textures);
	hyd_list_init(&engine->input_presets);

	return engine;
}

uint8_t hyd_engine_init(struct hyd_engine *engine, const char *argv[])
{
	if (init_sdl(&engine->window, &engine->renderer, 600, 480) != 0)
		return 1;

	SDL_SetRenderDrawBlendMode(engine->renderer, SDL_BLENDMODE_ADD);
}

void hyd_engine_events(struct hyd_engine *engine)
{
	if (engine == NULL)
		return;

	SDL_Event event;
	while (SDL_PollEvent(&event)) {
		if (event.type == SDL_QUIT)
			engine->running = 0;
		else if (event.type == SDL_KEYDOWN) {
			if (engine->current_input_preset == NULL)
				continue;

			struct hyd_input *iter;
			hyd_list_for_each_entry(iter,
					&engine->current_input_preset->inputs,
					list)
			{
				if (iter->callback != NULL &&
						iter->type == KEY &&
						iter->code == event.key.keysym.scancode
				   )
					iter->callback(engine, "down");
			}
		} else if (event.type == SDL_KEYUP) {
			if (engine->current_input_preset == NULL)
				continue;

			struct hyd_input *iter;
			hyd_list_for_each_entry(iter,
					&engine->current_input_preset->inputs,
					list)
			{
				if (iter->callback != NULL &&
						iter->type == KEY &&
						iter->code == event.key.keysym.scancode
					)
					iter->callback(engine, "up");
			}
		}
	}
}

void hyd_engine_destroy(struct hyd_engine *engine)
{
	if (engine == NULL)
		return;

	if (engine->current_mod != NULL &&
			engine->current_mod->destroy != NULL)
		engine->current_mod->destroy(engine);

	struct hyd_input_preset *preset_iter, *preset_next;
	hyd_list_for_each_entry_safe(preset_iter, preset_next,
			&engine->input_presets, list)
	{
		hyd_input_preset_destroy(preset_iter);
	}

	hyd_scene_destroy(engine->current_scene);
	hyd_tex_list_destroy(&engine->textures);
	hyd_mod_destroy(engine->current_mod);

	if (engine->renderer != NULL)
		SDL_DestroyRenderer(engine->renderer);
	if (engine->window != NULL)
		SDL_DestroyWindow(engine->window);
	SDL_Quit();

	free(engine);
}

void hyd_engine_draw(struct hyd_engine *engine)
{
	hyd_scene_draw(engine->current_scene, engine->renderer);
}

void hyd_engine_update(struct hyd_engine *engine, uint32_t dt)
{
	if (engine == NULL)
		return;

	if (engine->current_mod != NULL &&
			engine->current_mod->update != NULL)
		engine->current_mod->update(engine, dt);
}

uint8_t hyd_engine_run(struct hyd_engine *engine)
{
	uint32_t last_time = SDL_GetTicks(), current_time, dt;
	while (engine->running) {
		engine_events(engine);

		current_time = SDL_GetTicks();
		dt = (current_time - last_time);
		last_time = current_time;

		SDL_RenderClear(engine->renderer);

		hyd_engine_update(engine, dt);

		hyd_engine_draw(engine);

		SDL_RenderPresent(engine->renderer);
	}
}

uint8_t hyd_engine_load_scene(struct hyd_engine *engine, const char *filename)
{
	engine->current_scene = hyd_scene_create_file(filename,
			&engine->textures, engine->renderer);

	if (engine->current_scene == NULL)
		return 1;

	return 0;
}

uint8_t hyd_engine_load_input_preset(struct hyd_engine *engine, const char *filename)
{
	if (hyd_input_preset_list_create_file(&engine->input_presets,
				filename) == 0)
		engine->current_input_preset =
			hyd_list_entry(engine->input_presets.next,
					typeof(*(engine->current_input_preset)),
					list);
	else
		return 1;

	if (engine->current_input_preset == NULL)
		return 1;

	return 0;
}

uint8_t hyd_engine_load_mod(struct hyd_engine *engine, const char *filename)
{
	struct hyd_mod_info *info = hyd_mod_info_get(filename);
	engine->current_mod = hyd_mod_create(info);
	if (engine->current_mod != NULL &&
			engine->current_mod->init != NULL)
		engine->current_mod->init(engine);
	else
		return 1;

	if (engine->current_mod == NULL)
		return 1;

	return 0;
}

struct hyd_scene *hyd_engine_get_current_scene(struct hyd_engine *engine)
{
	return engine->current_scene;
}

struct hyd_input_preset *hyd_engine_get_current_input_preset(struct hyd_engine *engine)
{
	return engine->current_input_preset;
}

SDL_Renderer *hyd_engine_get_renderer(struct hyd_engine *engine)
{
	return engine->renderer;
}
