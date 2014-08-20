module hyd.text;

import hyd.engine;
import hyd.geom;
import hyd.color;

extern (C) nothrow {

struct hyd_font {
	void *face;
}

struct hyd_text {
	bool ready;
	char[] text;
	SDL_Color color;
	void *tex;
	hyd_font *font;
	SDL_Rect src;
}

struct hyd_locale {
	char *name;
	uint num_texts;
	hyd_text **texts;
	hyd_locale *next;
	hyd_locale *children;
}

hyd_font *hyd_font_create_file(const char *fname);

hyd_text *hyd_text_create(const char *str, const char *name);

ubyte hyd_text_render(hyd_text *t, hyd_font *font);

ubyte hyd_locale_create_file(hyd_locale *l, const char *fname);

ubyte hyd_locale_create_json(hyd_locale *l, void *root, const char *name);

hyd_locale *hyd_locale_find(hyd_locale *l, const char *name);

hyd_text *hyd_locale_find_text(hyd_locale *l, const char *name);

ubyte hyd_locale_render(hyd_locale *l, hyd_font *font);

ubyte hyd_text_draw(hyd_text *text,
					float x, float y,
					hyd_color c);

bool hyd_text_draw_str(	hyd_font *font,
						const char *str,
						float x, float y,
						hyd_color c);

void hyd_font_destroy(hyd_font *font);

void hyd_text_destroy(hyd_text *text);
}
