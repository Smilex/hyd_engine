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


struct hyd_engine *_hyd_engine = NULL;
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

struct hyd_engine *hyd_engine_get(void) {
	return _hyd_engine;
}

uint8_t hyd_engine_init(const char *argv[])
{
	_hyd_engine = malloc(sizeof(*_hyd_engine));
	if (_hyd_engine == NULL)
		return 1;

	_hyd_engine->running = 1;
	_hyd_engine->quit = 0;
	_hyd_engine->pause = 0;
	_hyd_engine->current_scene = NULL;
	_hyd_engine->curr_ip = NULL;
	_hyd_engine->current_mod = NULL;
	_hyd_engine->context = NULL;
	_hyd_engine->window = NULL;
	_hyd_engine->ip_head = malloc(sizeof(*_hyd_engine->ip_head));
	_hyd_engine->ip_head->next = _hyd_engine->ip_head;
	_hyd_engine->tex_head = malloc(sizeof(*_hyd_engine->tex_head));
	_hyd_engine->tex_head->next = _hyd_engine->tex_head;
	_hyd_engine->locale_head = malloc(sizeof(*_hyd_engine->locale_head));
	_hyd_engine->locale_head->next = _hyd_engine->locale_head;


	if (PHYSFS_init(argv[0]) == 0)
		return 1;

	uint32_t width = 800, height = 600;
	if (hyd_init_sdl(&_hyd_engine->window, &_hyd_engine->context, width, height) != 0) {
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

void hyd_engine_events(void)
{
	if (_hyd_engine == NULL)
		return;

	SDL_Event event;
	while (SDL_PollEvent(&event)) {
		if (event.type == SDL_QUIT) {
			_hyd_engine->running = 0;
			_hyd_engine->quit = 1;
		}
	}
}

void hyd_engine_destroy(void)
{
	if (_hyd_engine == NULL)
		return;

	if (_hyd_engine->current_mod != NULL &&
			_hyd_engine->current_mod->destroy != NULL)
		_hyd_engine->current_mod->destroy(_hyd_engine);

	struct hyd_ip *preset_iter, *preset_next;
	for (preset_iter = _hyd_engine->ip_head->next, preset_next = preset_iter->next;
			preset_iter != _hyd_engine->ip_head;
			preset_iter = preset_next, preset_next = preset_iter->next) 
	{
		hyd_ip_destroy(preset_iter);
	}

	hyd_scene_destroy(_hyd_engine->current_scene);
	hyd_tex_list_destroy(_hyd_engine->tex_head);
	hyd_mod_destroy(_hyd_engine->current_mod);


	if (PHYSFS_isInit() != 0)
		PHYSFS_deinit();
	if (_hyd_engine->context != NULL)
		SDL_GL_DeleteContext(_hyd_engine->context);
	if (_hyd_engine->window != NULL)
		SDL_DestroyWindow(_hyd_engine->window);
	SDL_Quit();

	free(_hyd_engine);
}

void hyd_engine_draw(void)
{
	if (_hyd_engine == NULL)
		return;

	hyd_scene_draw(_hyd_engine->current_scene);
}

void hyd_engine_update(uint32_t dt)
{
	if (_hyd_engine == NULL)
		return;
}

void hyd_engine_begin_draw(void) {
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

void hyd_engine_end_draw() {
	SDL_GL_SwapWindow(_hyd_engine->window);
}

uint8_t hyd_engine_load_scene(const char *filename)
{
	_hyd_engine->current_scene = hyd_scene_create_file(filename,
			_hyd_engine->tex_head);

	if (_hyd_engine->current_scene == NULL)
		return 1;

	return 0;
}

uint8_t hyd_engine_load_ip(const char *filename)
{
	if (_hyd_engine->ip_head != NULL) {
		struct hyd_ip *i, *n;
		for (i = _hyd_engine->ip_head->next, n = i->next;
				i != _hyd_engine->ip_head;
				i = n, n = i->next) 
		{
			hyd_ip_destroy(i);
		}
	}

	if (hyd_ip_create_file(_hyd_engine->ip_head,
				filename) == 0)
		_hyd_engine->curr_ip = _hyd_engine->ip_head->next;
	else
		return 1;

	if (_hyd_engine->curr_ip == NULL)
		return 1;

	return 0;
}

uint8_t hyd_engine_load_locale(const char *filename)
{
	if (hyd_locale_create_file(_hyd_engine->locale_head, filename))
		return 1;

	return 0;
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

struct hyd_scene *hyd_engine_get_scene(void) {
	return _hyd_engine->current_scene;
}

struct hyd_ip *hyd_engine_get_ip(void) {
	return _hyd_engine->curr_ip;
}

struct hyd_locale *hyd_engine_get_locale(void) {
	return _hyd_engine->locale_head;
}

struct hyd_tex_list *hyd_engine_get_tex_list(void) {
	return _hyd_engine->tex_head;
}

uint8_t hyd_engine_get_quit() {
	return _hyd_engine->quit;
}
