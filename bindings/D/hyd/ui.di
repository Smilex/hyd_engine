module hyd.ui;
import hyd.quad;
import hyd.color;
import hyd.text;

extern (C) nothrow {

enum hyd_btn_state {
	NONE, HOVER, DOWN_L, DOWN_M, DOWN_R
}

void hyd_ui_set_cursor(float x, float y);

hyd_btn_state hyd_ui_btn(float w, float h);

hyd_btn_state hyd_ui_text(hyd_text *t, hyd_color *c, float w, float h);

}
