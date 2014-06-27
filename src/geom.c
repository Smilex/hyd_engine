#include "geom.h"

void hyd_hex_color(uint32_t hex, uint8_t *r, uint8_t *g, uint8_t *b) {
	*r = ((hex >> 16) & 0xFF);
	*g = ((hex >> 8) & 0xFF);
	*b = ((hex) & 0xFF);
}

void hyd_geom_draw_line(SDL_Renderer *rend, SDL_Point p1, SDL_Point p2,
		uint32_t hex, uint8_t opacity) {
	uint8_t r, g, b, a = opacity;
	hyd_hex_color(hex, &r, &g, &b);

	SDL_SetRenderDrawColor(rend, r, g, b, a);
	SDL_RenderDrawLine(rend, p1.x, p1.y, p2.x, p2.y);
	SDL_SetRenderDrawColor(rend, 0, 0, 0, 0xFF);
}

void hyd_geom_draw_rect(SDL_Renderer *rend, SDL_Rect rect,
		uint32_t hex, uint8_t opacity, uint8_t fill) {
	uint8_t r, g, b, a = opacity;
	hyd_hex_color(hex, &r, &g, &b);

	SDL_SetRenderDrawColor(rend, r, g, b, a);
	if (fill)
		SDL_RenderFillRect(rend, &rect);
	else
		SDL_RenderDrawRect(rend, &rect);
	SDL_SetRenderDrawColor(rend, 0, 0, 0, 0xFF);
}
