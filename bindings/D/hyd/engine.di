module hyd.engine;
import hyd.scene;

extern (C) nothrow {

alias void function (hyd_engine*, uint) update_func;
alias void function (hyd_engine*) draw_func;

struct hyd_engine {
	bool running;

	void *renderer;
	void *window;

	update_func call_update;
	draw_func call_draw;

	hyd_scene *current_scene;
	void *current_mod;
	void *current_input_preset;
}

hyd_engine *hyd_engine_create();

bool hyd_engine_init(hyd_engine *engine, char **argv);

void hyd_engine_destroy(hyd_engine *engine);

void hyd_engine_events(hyd_engine *engine);

void hyd_engine_update(hyd_engine *engine, uint dt);

void hyd_engine_draw(hyd_engine *engine);

bool hyd_engine_run(hyd_engine *engine);

bool hyd_engine_load_scene(hyd_engine *engine, const char *filename);

bool hyd_engine_load_input_preset(hyd_engine *engine, const char *filename);

bool hyd_engine_load_mod(hyd_engine *engine, const char[] filename);

void hyd_engine_update_func(hyd_engine *e, update_func f);
void hyd_engine_draw_func(hyd_engine *e, draw_func f);
}
