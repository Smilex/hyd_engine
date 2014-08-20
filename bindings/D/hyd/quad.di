module hyd.quad;
import hyd.color;

extern (C) nothrow {

struct hyd_quad {
	float x1;
	float y1;
	float x2;
	float y2;
}

void hyd_quad_draw(hyd_quad *q, hyd_color *c);

}
