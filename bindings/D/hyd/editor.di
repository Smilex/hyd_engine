module hyd.editor;
import hyd.quad;
import hyd.gfx;
import hyd.text;
import hyd.transform;
import hyd.scene;

extern (C) nothrow {

struct hyd_editor {
	hyd_program *argb_shdr;
	hyd_program *text_shdr;
	hyd_quad area;
	hyd_font *font;
	hyd_locale *locale;
	hyd_transform *view;
	hyd_scene *scene;
}

ubyte hyd_editor_init(hyd_transform *view, hyd_scene *scene);

hyd_editor *hyd_editor_create(hyd_quad area, hyd_program *argb, hyd_program *text, hyd_font *font,
	hyd_locale *locale, hyd_transform *view);

ubyte hyd_editor_open();

ubyte hyd_editor_update();

void hyd_editor_draw();

void hyd_editor_ui();

}
