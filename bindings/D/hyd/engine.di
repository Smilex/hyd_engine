module hyd.engine;
import hyd.scene;
import hyd.input;
import hyd.text;
import hyd.gfx;

extern (C) nothrow {

alias void function (hyd_engine*, uint) update_func;
alias void function (hyd_engine*) draw_func;

struct hyd_engine {
	bool running;
	bool quit;
	bool pause;

	void *renderer;
	void *window;

	hyd_scene *current_scene;
	void *current_mod;
	hyd_ip *curr_ip;
	hyd_ip *ip_head;
	hyd_locale *locale_head;

	void *tex_prog;
	void *argb_prog;
}

hyd_engine *hyd_engine_get();

bool hyd_engine_init(char **argv, uint w, uint h);

void hyd_engine_destroy();

void hyd_engine_events();

void hyd_engine_update(uint dt);

void hyd_engine_draw();

void hyd_engine_begin_draw();

void hyd_engine_end_draw();

bool hyd_engine_run();

bool hyd_engine_load_scene(const char *filename);

bool hyd_engine_load_ip(const char *filename);

ubyte hyd_engine_load_locale(const char *filename);

uint hyd_engine_get_time();

void hyd_engine_set_tex_shdr(void *s);

void hyd_engine_set_argb_shdr(void *s);

hyd_program *hyd_tex_shdr();

hyd_program *hyd_argb_shdr();

hyd_program *hyd_gray_shdr();

hyd_scene *hyd_engine_get_scene();

hyd_ip *hyd_engine_get_ip();

hyd_locale *hyd_engine_get_locale();

void *hyd_engine_get_tex_list();

bool hyd_engine_get_quit();

}
