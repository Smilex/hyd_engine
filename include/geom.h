/**
 * \file geom.h
 */

#ifndef HYD_ENGINE_H
#define HYD_ENGINE_H

#include <SDL.h>

void hyd_hex_color(uint32_t hex, uint8_t *r, uint8_t *g, uint8_t *b);

void hyd_geom_draw_line(SDL_Renderer *rend, SDL_Point p1, SDL_Point p2,
		uint32_t hex, uint8_t opacity);

void hyd_geom_draw_rect(SDL_Renderer *rend, SDL_Rect rect,
		uint32_t hex, uint8_t opacity, uint8_t fill);

#endif
