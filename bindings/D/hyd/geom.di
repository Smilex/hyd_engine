module hyd.geom;

extern (C) {

struct SDL_Point {
	int x;
	int y;
}

struct SDL_Rect {
	int x;
	int y;
	int w;
	int h;
}

struct SDL_Color {
	ubyte r;
	ubyte g;
	ubyte b;
	ubyte a;
}

void hyd_hex_color(uint hex, ubyte *r, ubyte *g, ubyte *b);

void hyd_geom_draw_line(void *rend, SDL_Point p1, SDL_Point p2,
		uint hex, ubyte opacity);

void hyd_geom_draw_rect(void *rend, SDL_Rect rect,
		uint hex, ubyte opacity, bool fill);

}
