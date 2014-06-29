#include "engine.h"
#include "init.h"

#include <SDL_ttf.h>

struct hyd_engine *hyd_engine_create(void)
{
	struct hyd_engine *engine = malloc(sizeof(*engine));
	if (engine == NULL)
		return NULL;

	engine->running = 1;
	engine->quit = 0;
	engine->pause = 0;
	engine->current_scene = NULL;
	engine->curr_ip = NULL;
	engine->current_mod = NULL;
	engine->renderer = NULL;
	engine->window = NULL;
	engine->call_update = NULL;
	engine->call_draw = NULL;
	engine->ip_head = malloc(sizeof(*engine->ip_head));
	engine->ip_head->next = engine->ip_head;
	engine->tex_head = malloc(sizeof(*engine->tex_head));
	engine->tex_head->next = engine->tex_head;
	engine->locale_head = malloc(sizeof(*engine->locale_head));
	engine->locale_head->next = engine->locale_head;

	return engine;
}

uint8_t hyd_engine_init(struct hyd_engine *engine, const char *argv[])
{
	if (PHYSFS_init(argv[0]) == 0)
		return 1;

	if (init_sdl(&engine->window, &engine->renderer, 600, 480) != 0)
		return 1;

	if (TTF_Init() == -1)
		return 1;

	SDL_SetRenderDrawBlendMode(engine->renderer, SDL_BLENDMODE_ADD);

	return 0;
}

void hyd_engine_events(struct hyd_engine *engine)
{
	if (engine == NULL)
		return;

	struct hyd_ip *ip = engine->curr_ip;
	SDL_Event event;
	while (SDL_PollEvent(&event)) {
		if (event.type == SDL_QUIT) {
			engine->running = 0;
			engine->quit = 1;
		}
		else if (event.type == SDL_KEYDOWN) {
			if (engine->curr_ip == NULL)
				continue;

			uint32_t i;
			for(i = 0;i < ip->count;i++) {
				if (ip->inputs[i].callback != NULL &&
						ip->inputs[i].type == KEY &&
						ip->inputs[i].code == event.key.keysym.scancode
				   )
					ip->inputs[i].callback(engine, "down");
			}
		} else if (event.type == SDL_KEYUP) {
			if (engine->curr_ip == NULL)
				continue;

			uint32_t i;
			for(i = 0;i < ip->count;i++) {
				if (ip->inputs[i].callback != NULL &&
						ip->inputs[i].type == KEY &&
						ip->inputs[i].code == event.key.keysym.scancode
				   )
					ip->inputs[i].callback(engine, "up");
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

	struct hyd_ip *preset_iter, *preset_next;
	for (preset_iter = engine->ip_head->next, preset_next = preset_iter->next;
			preset_iter != engine->ip_head;
			preset_iter = preset_next, preset_next = preset_iter->next) 
	{
		hyd_ip_destroy(preset_iter);
	}

	hyd_scene_destroy(engine->current_scene);
	hyd_tex_list_destroy(engine->tex_head);
	hyd_mod_destroy(engine->current_mod);


	if (TTF_WasInit())
		TTF_Quit();
	if (PHYSFS_isInit() != 0)
		PHYSFS_deinit();
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

	if (engine->call_draw != NULL)
		engine->call_draw(engine);
}

void hyd_engine_update(struct hyd_engine *engine, uint32_t dt)
{
	if (engine == NULL)
		return;

	if (engine->call_update != NULL)
		engine->call_update(engine, dt);

	if (engine->current_mod != NULL &&
			engine->current_mod->update != NULL)
		engine->current_mod->update(engine, dt);
}

uint8_t hyd_engine_run(struct hyd_engine *engine)
{
	uint32_t last_time = SDL_GetTicks(), current_time, dt;
	while (engine->running) {
		hyd_engine_events(engine);

		current_time = SDL_GetTicks();
		dt = (current_time - last_time);
		last_time = current_time;

		SDL_RenderClear(engine->renderer);

		hyd_engine_update(engine, dt);

		hyd_engine_draw(engine);

		SDL_RenderPresent(engine->renderer);
	}
}

void hyd_engine_begin_draw(struct hyd_engine *e) {
	SDL_RenderClear(e->renderer);
}

void hyd_engine_end_draw(struct hyd_engine *e) {
	SDL_RenderPresent(e->renderer);
}

uint8_t hyd_engine_load_scene(struct hyd_engine *engine, const char *filename)
{
	engine->current_scene = hyd_scene_create_file(filename,
			engine->tex_head, engine->renderer);

	if (engine->current_scene == NULL)
		return 1;

	return 0;
}

uint8_t hyd_engine_load_ip(struct hyd_engine *engine, const char *filename)
{
	if (engine->ip_head != NULL) {
		struct hyd_ip *i, *n;
		for (i = engine->ip_head->next, n = i->next;
				i != engine->ip_head;
				i = n, n = i->next) 
		{
			hyd_ip_destroy(i);
		}
	}

	if (hyd_ip_create_file(engine->ip_head,
				filename) == 0)
		engine->curr_ip = engine->ip_head->next;
	else
		return 1;

	if (engine->curr_ip == NULL)
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

uint8_t hyd_engine_load_locale(struct hyd_engine *engine, const char *filename)
{
	if (hyd_locale_create_file(engine->locale_head, filename))
		return 1;

	return 0;
}

void hyd_engine_update_func(struct hyd_engine *e, void (*f)(struct hyd_engine*,uint32_t))
{
	e->call_update = f;
}

void hyd_engine_draw_func(struct hyd_engine *e, void (*f)(struct hyd_engine*))
{
	e->call_draw = f;
}

uint32_t hyd_engine_get_time(void)
{
	return SDL_GetTicks();
}
