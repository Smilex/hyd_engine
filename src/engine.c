#include "engine.h"
#include "init.h"
#include "input.h"

#include "gl_core_3_3.h"
#include "graphics.h"
#include "matrix.h"
#include "geom.h"
#include "quad.h"

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

const GLchar *_hyd_tex_vertex =
	"#version 120\n"
	"attribute vec2 position;"
	"attribute vec4 color;"
	"attribute vec2 uv;"
	"uniform mat4 model_mat;"
	"varying vec2 _uv;"
	"varying vec4 _color;"
	"void main() {"
	"_uv = uv;"
	"_color = color;"
	"gl_Position = model_mat * vec4(position, 0.0, 1.0);"
	"}";

const GLchar *_hyd_tex_frag =
	"#version 120\n"
	"varying vec2 _uv;"
	"varying vec4 _color;"
	"uniform sampler2D tex;"
	"void main() {"
	"gl_FragColor = texture2D(tex, _uv) * _color;"
	"}";

const GLchar *_hyd_gray_frag =
	"#version 120\n"
	"varying vec2 _uv;"
	"varying vec4 _color;"
	"uniform sampler2D tex;"
	"void main() {"
	"float alpha = texture2D(tex, _uv).r;"
	"gl_FragColor = vec4(_color.rgb, _color.a * alpha);"
	"}";

const GLchar *_hyd_argb_vertex =
	"#version 120\n"
	"attribute vec2 position;\n"
	"attribute vec4 color;\n"
	"uniform mat4 proj_mat;\n"
	"uniform mat4 model_mat;\n"
	"varying vec4 col;\n"
	"void main() {\n"
	"col = color;\n"
	"gl_Position = model_mat * vec4(position, 0.0, 1.0);\n"
	"}";

const GLchar *_hyd_argb_frag =
	"#version 120\n"
	"varying vec4 col;"
	"void main() {\n"
	"gl_FragColor = col;\n"
	"}";


GLuint last_id = 0;

void gl_debug(	GLenum source,
				GLenum type,
				GLuint id,
				GLenum severity,
				GLsizei length,
				const GLchar *msg,
				const void *user)
{
	if (id != last_id) {
		printf("%s\n", msg);
		last_id = id;
	}
}

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
	engine->context = NULL;
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

	uint32_t width = 800, height = 600;
	if (hyd_init_sdl(&engine->window, &engine->context, width, height) != 0) {
		printf("%s", SDL_GetError());
		return 1;
	}

	if (ogl_LoadFunctions() == ogl_LOAD_FAILED)
		return 1;

	if (ogl_ext_KHR_debug != ogl_LOAD_FAILED)
		glDebugMessageCallback(gl_debug, NULL);

	glClearColor(0.0f, 0.0f, 0.1f, 1.0f);
	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LEQUAL);

	hyd_input_load_controllers();

	return 0;
}

void hyd_engine_events(struct hyd_engine *engine)
{
	if (engine == NULL)
		return;

	SDL_Event event;
	while (SDL_PollEvent(&event)) {
		if (event.type == SDL_QUIT) {
			engine->running = 0;
			engine->quit = 1;
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


	if (PHYSFS_isInit() != 0)
		PHYSFS_deinit();
	if (engine->context != NULL)
		SDL_GL_DeleteContext(engine->context);
	if (engine->window != NULL)
		SDL_DestroyWindow(engine->window);
	SDL_Quit();

	free(engine);
}

void hyd_engine_draw(struct hyd_engine *engine)
{
	hyd_scene_draw(engine->current_scene);

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

void hyd_engine_begin_draw(struct hyd_engine *e) {
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

void hyd_engine_end_draw(struct hyd_engine *e) {
	SDL_GL_SwapWindow(e->window);
}

uint8_t hyd_engine_load_scene(struct hyd_engine *engine, const char *filename)
{
	engine->current_scene = hyd_scene_create_file(filename,
			engine->tex_head);

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

struct hyd_program *hyd_tex_shdr(void) {
	hyd_program_impl();
		hyd_shader_impl();
			hyd_shader_type(GL_VERTEX_SHADER);
			hyd_shader_source(_hyd_tex_vertex);
		hyd_program_attach(hyd_shader_finish());
		hyd_shader_impl();
			hyd_shader_type(GL_FRAGMENT_SHADER);
			hyd_shader_source(_hyd_tex_frag);
		hyd_program_attach(hyd_shader_finish());

		hyd_program_bind_attrib("position", 0);
		hyd_program_bind_attrib("color", 1);
		hyd_program_bind_attrib("uv", 2);
	return hyd_program_finish();
}

struct hyd_program *hyd_argb_shdr(void) {
	hyd_program_impl();
		hyd_shader_impl();
			hyd_shader_type(GL_VERTEX_SHADER);
			hyd_shader_source(_hyd_argb_vertex);
		hyd_program_attach(hyd_shader_finish());
		hyd_shader_impl();
			hyd_shader_type(GL_FRAGMENT_SHADER);
			hyd_shader_source(_hyd_argb_frag);
		hyd_program_attach(hyd_shader_finish());

		hyd_program_bind_attrib("position", 0);
		hyd_program_bind_attrib("color", 1);
	return hyd_program_finish();
}

struct hyd_program *hyd_gray_shdr(void) {
	hyd_program_impl();
		hyd_shader_impl();
			hyd_shader_type(GL_VERTEX_SHADER);
			hyd_shader_source(_hyd_tex_vertex);
		hyd_program_attach(hyd_shader_finish());
		hyd_shader_impl();
			hyd_shader_type(GL_FRAGMENT_SHADER);
			hyd_shader_source(_hyd_gray_frag);
		hyd_program_attach(hyd_shader_finish());

		hyd_program_bind_attrib("position", 0);
		hyd_program_bind_attrib("color", 1);
		hyd_program_bind_attrib("uv", 2);
	return hyd_program_finish();
}
