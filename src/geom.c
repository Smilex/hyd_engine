#include "geom.h"
#include "graphics.h"

void hyd_hex_color(uint32_t hex, uint8_t *r, uint8_t *g, uint8_t *b) {
	*r = ((hex >> 16) & 0xFF);
	*g = ((hex >> 8) & 0xFF);
	*b = ((hex) & 0xFF);
}

void hyd_geom_draw_line(SDL_Point p1, SDL_Point p2,
		uint32_t hex, uint8_t opacity) {
	uint8_t r, g, b, a = opacity;
	hyd_hex_color(hex, &r, &g, &b);

	/*float verts[] = {p1.x, p1.y, p2.x, p2.y};
	hyd_vbo_impl();
		hyd_vbo_data(verts, sizeof(verts));
	struct hyd_vbo *v = hyd_vbo_finish();
	hyd_vbo_bind(v);

	hyd_program_use(_hyd_argb_shdr);
	hyd_gfx_draw(GL_LINES, 4);

	glDeleteBuffers(1, &v->ptr);
	free(v);
	*/
}

void hyd_geom_draw_rect(SDL_Rect rect,
		uint32_t hex, uint8_t opacity, uint8_t fill) {
	uint8_t r, g, b, a = opacity;
	hyd_hex_color(hex, &r, &g, &b);
}
